#define PERFORM_COMPUTATION

#define NPAD 1

#define KXL get_local_id(0)
#define KYL get_local_id(1)

#define TILE_OFFSET(base, idx, size) ( base + idx*size.s0*size.s1 )

static inline int region_idx(int4 halo, int2 size)
{
   return (KXL+halo.s0) + (KYL+halo.s2)*size.s0;
}


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
   const int lStride = get_local_size(0)  + 2*NPAD;
   const int gStride = get_global_size(0) + 2*NPAD;

   // offset to central (non-extended) portion of array
   //
   int g_ex_offset =   ( get_group_id(0) * get_local_size(0) )
                     + ( get_group_id(1) * get_local_size(1) ) * gStride;

   const int kl = KXL + KYL*lStride;
   const int k  = KXL + KYL*gStride + g_ex_offset;

   // top-left portion
   //
   patch[kl] = A[k];

   // right section
   //
   if (KXL < 2*NPAD) {
      patch[kl + get_local_size(0)] = A[k + get_local_size(0)];
   }

   // bottom section
   //
   if (KYL < 2*NPAD) {
      patch[kl + get_local_size(1)*lStride] = A[k + get_local_size(1)*gStride];

      if (KXL < 2*NPAD) {
         patch[kl + get_local_size(0) + get_local_size(1)*lStride]
           = A[k  + get_local_size(0) + get_local_size(1)*gStride];
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

   const int pOffset = NPAD + NPAD*lStride;

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

#else

   C[k] = patch[kl];

#endif
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