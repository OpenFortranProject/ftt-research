#define PERFORM_SHIFT

// allow data type of buffers to change
//
typedef float ofp_data_t;

static inline uint k_index()
{
   return ((uint)get_global_id(0) + (uint)get_global_size(0) * (uint)get_global_id(1));
}

static inline uint kl_index()
{
   return ((uint)get_local_id(0) + (uint)get_local_size(0) * (uint)get_local_id(1));
}

static inline uint global_count()
{
   return ((uint)get_global_size(0) * (uint)get_global_size(1));
}

static inline uint global_size_ex(uint dimindx, uint nPad)
{
   return ((uint)get_global_size(dimindx) + 2*nPad);
}

/**
 * copy shifted region of A to local patch
 */
static inline void shift_to_patch(__local  ofp_data_t * patch,
                                  __global ofp_data_t * A,
                                  int xShift, int yShift, int nPad)
{
   uint kxl, kyl;

   const uint pStride = (uint)get_local_size(0); // patch is not padded for shift
   const uint aStride = (uint)get_global_size(0) + 2*nPad;

   // offset into extended array based on thread group location
   //
   const int offset =   ( (uint)get_group_id(0) * (uint)get_local_size(0) + nPad )
                      + ( (uint)get_group_id(1) * (uint)get_local_size(1) + nPad ) * aStride;

   // copy shifted array to patch (all threads participate)
   //
   kxl = get_local_id(0);
   kyl = get_local_id(1);

   patch[kxl + kyl*pStride] = A[(kxl-xShift) + (kyl-yShift)*aStride + offset];
}


/**
 * copy all shifted regions of A to local padded subsection
 */
static inline void copy_shifts_subsection(__local  ofp_data_t * patch,
                                          __global ofp_data_t * A,
                                          int pPad, int aPad)
{
   const int pStride = (int)get_local_size(0)  + 2*pPad;
   const int aStride = (int)get_global_size(0) + 2*aPad;

   // offset to central (non-extended) portion of array
   //
   const int aOffset =   ( (int)get_group_id(0) * (int)get_local_size(0) + aPad )
                       + ( (int)get_group_id(1) * (int)get_local_size(1) + aPad ) * aStride;

   // offset to central (non-extended) portion of patch
   //
   const int pOffset = pPad + pPad*pStride;

   int kxl = (int)get_local_id(0);
   int kyl = (int)get_local_id(1);

   // copy central subsection (all threads participate)
   //
   patch[kxl + kyl*pStride + pOffset] = A[kxl + kyl*aStride + aOffset];

   // copy top and bottom subsections
   //
   if (kyl < pPad) {
      kyl -= pPad;
      patch[kxl + kyl*pStride + pOffset] = A[kxl + kyl*aStride + aOffset];
      kyl += pPad;

      kyl += (int)get_local_size(1);
      patch[kxl + kyl*pStride + pOffset] = A[kxl + kyl*aStride + aOffset];
      kyl -= (int)get_local_size(1);
   }

   // copy left and right subsections
   //
   if (kxl < pPad) {
      kxl -= pPad;
      patch[kxl + kyl*pStride + pOffset] = A[kxl + kyl*aStride + aOffset];
      kxl += pPad;

      kxl += (int)get_local_size(0);
      patch[kxl + kyl*pStride + pOffset] = A[kxl + kyl*aStride + aOffset];
      kxl -= (int)get_local_size(0);
   }
}


/**
 * computes the average of surrounding array elements
 */
__kernel void shift (int nShift,
          __global ofp_data_t * A,
          __global ofp_data_t * B,
          __global ofp_data_t * C,
          __local  ofp_data_t * patch )
{
   // TEMP - need to know size of extended array (rightPad?)
   const int nPad = 1;

   const int kxl = (int)get_local_id(0);
   const int kyl = (int)get_local_id(1);

   const int k  = (get_global_id(0) + nPad)
                + (get_global_id(1) + nPad) * (get_global_size(0) + 2*nPad);

   const int pStride = (int)get_local_size(0) + 2*nShift;
   const int pOffset = nShift + nShift*pStride;

   // copy all shifted subsections into a single extended local region
   //

#ifdef PERFORM_SHIFT
   copy_shifts_subsection(patch, A, nShift, nPad);
   barrier(CLK_LOCAL_MEM_FENCE);

   // probably should test for nxGlobal, nyGlobal
   //
   C[k] = ( C[k]
        + patch[(kxl-nShift) + kyl*pStride + pOffset]
        +   patch[(kxl+nShift) + kyl*pStride + pOffset]
        +   patch[kxl + (kyl-nShift)*pStride + pOffset]
        +   patch[kxl + (kyl+nShift)*pStride + pOffset]
          ) / 5.0;
#else
   C[k] = patch[kxl];
#endif

}


/**
 * computes the average of surrounding array elements
 */
__kernel void shift_individually (int nShift,
          __global ofp_data_t * A,
          __global ofp_data_t * B,
          __global ofp_data_t * C,
          __local  ofp_data_t * patch )
{
   // TEMP - need to know size of extended array (rightPad?)
   const int nPad = 1;

   const int kl = kl_index();
   const int kx = get_global_id(0) + nPad;
   const int ky = get_global_id(1) + nPad;
   const int k  = kx + ky*(get_global_size(0) + 2*nPad);

   // left
   //
   shift_to_patch(patch, A, -nShift, 0, nPad);
   barrier(CLK_LOCAL_MEM_FENCE);

   if (kx < get_global_size(0) + 2*nPad  &&  ky < get_global_size(1) + 2*nPad) {
      C[k] = patch[kl];
   }

   // right
   //
   shift_to_patch(patch, A, +nShift, 0, nPad);
   barrier(CLK_LOCAL_MEM_FENCE);

   if (kx < get_global_size(0) + 2*nPad  &&  ky < get_global_size(1) + 2*nPad) {
      C[k] = patch[kl] + C[k];
   }

   // up
   //
   shift_to_patch(patch, A, 0, -nShift, nPad);
   barrier(CLK_LOCAL_MEM_FENCE);

   if (kx < get_global_size(0) + 2*nPad  &&  ky < get_global_size(1) + 2*nPad) {
      C[k] = patch[kl] + C[k];
   }

   // down
   //
   shift_to_patch(patch, A, 0, +nShift, nPad);
   barrier(CLK_LOCAL_MEM_FENCE);

   if (kx < get_global_size(0) + 2*nPad  &&  ky < get_global_size(1) + 2*nPad) {
      C[k] = (patch[kl] + C[k]) / 1;
   }

}
