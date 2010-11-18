#define CODE_INLINE
#define TOP_LEFT
#undef PERFORM_COMPUTATION
#undef DO_PADDING

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
                                          int lPad, int gPad)
{
   const int lStride = get_local_size(0)  + 2*lPad;
   const int gStride = get_global_size(0) + 2*gPad;

   // offset to central (non-extended) portion of array
   //
   int gOffset =   ( get_group_id(0) * get_local_size(0) )
                 + ( get_group_id(1) * get_local_size(1) ) * gStride;

   // offset to central (non-extended) portion of patch
   //
   int lOffset = lPad + lPad*lStride;

   int kxl = get_local_id(0);
   int kyl = get_local_id(1);

   // top-left portion
   //
   patch[kxl + kyl*lStride] = A[kxl + kyl*gStride + gOffset];

   // right section
   //
   if (kxl < 2*lPad) {
      patch[kxl + get_local_size(0) + kyl*lStride]
        = A[kxl + get_local_size(0) + kyl*gStride + gOffset];
   }

   // bottom section
   //
   if (kyl < 2*lPad) {
      patch[kxl + (kyl + get_local_size(1))*lStride]
        = A[kxl + (kyl + get_local_size(1))*gStride + gOffset];

      if (kxl < 2*lPad) {
         patch[kxl + get_local_size(0) + (kyl + get_local_size(1))*lStride]
           = A[kxl + get_local_size(0) + (kyl + get_local_size(1))*gStride + gOffset];
      }
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
   const int nPad = nShift;

   int kxl = get_local_id(0);
   int kyl = get_local_id(1);

   const int k  = (get_global_id(0) + nPad)
                + (get_global_id(1) + nPad) * (get_global_size(0) + 2*nPad);

   const int lStride = get_local_size(0)  + 2*nShift;
   const int gStride = get_global_size(0) + 2*nPad;

   int gOffset =   ( get_group_id(0) * get_local_size(0) + nPad )
                 + ( get_group_id(1) * get_local_size(1) + nPad ) * gStride;

   const int kl  = kxl + kyl*get_local_size(0);

   // copy all shifted subsections into a single extended local region
   //
   copy_shifts_subsection(patch, A, nShift, nPad);
   barrier(CLK_LOCAL_MEM_FENCE);

#ifdef PERFORM_COMPUTATION
   // probably should test for nxGlobal, nyGlobal
   //
   C[k] = ( C[k]
        + patch[(kxl-nShift) + kyl*lStride + pOffset]
        +   patch[(kxl+nShift) + kyl*lStride + pOffset]
        +   patch[kxl + (kyl-nShift)*lStride + pOffset]
        +   patch[kxl + (kyl+nShift)*lStride + pOffset]
          ) / 5.0;
#endif

   C[k] = patch[kl];
}


#ifdef DONT_COMPILE


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

#endif