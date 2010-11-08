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
static inline void shift_to_patch(__local  int * patch,
                                  __global int * A,
                                  int xShift, yShift, int nPad)
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
 * computes the average of surrounding array elements
 */
__kernel void shift (int nShift,
          __global int * A,
          __global int * B,
          __global int * C,
          __local  int * patch )
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
