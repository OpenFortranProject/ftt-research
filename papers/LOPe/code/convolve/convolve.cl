//#include "/Users/rasmussn/ofp-research/papers/local-orientation/code/convolve/npad.h"

#define USE_LOCAL_MEM 1

#define NPAD  3
#define WSIZE 16

#define LSIZE ((WSIZE+2*NPAD)*(WSIZE+2*NPAD))

#define KX  get_global_id(0)
#define KY  get_global_id(1)
#define KXL get_local_id(0)
#define KYL get_local_id(1)

#define NX  get_global_size(0)
#define NY  get_global_size(1)

#define SQ(x)    ((x)*(x))

#define TILE_OFFSET(base, idx, size) ( base + idx*size.s0*size.s1 )

static inline int region_idx(int4 halo, int2 size)
{
   return (KXL+halo.s0) + (KYL+halo.s2)*size.s0;
}

static inline float transfer_halo(__global float * A, int4 halo, __local float * tile)
{
   const int lStride = get_local_size(0)  + halo.s0 + halo.s1;
   const int gStride = get_global_size(0) + halo.s0 + halo.s1;

   // offset to extended portion of array
   //
   const int g_ex_offset =   ( get_group_id(0) * get_local_size(0) )
                           + ( get_group_id(1) * get_local_size(1) ) * gStride;

   const int kl = KXL + KYL*lStride;
   const int k  = KXL + KYL*gStride + g_ex_offset;

   // top-left portion
   //
   tile[kl] = A[k];

   // right section
   //
   if (KXL < halo.s0+halo.s0) {
      tile[kl + get_local_size(0)] = A[k + get_local_size(0)];
   }

   // bottom section
   //
   if (KYL < halo.s2+halo.s3) {
      tile[kl + get_local_size(1)*lStride] = A[k + get_local_size(1)*gStride];

      if (KXL < halo.s0+halo.s1) {
         tile[kl + get_local_size(0) + get_local_size(1)*lStride]
          = A[k  + get_local_size(0) + get_local_size(1)*gStride];
      }
   }

   return tile[kl + halo.s0 + halo.s2*lStride];
}


/**
 * convolution filter
 */
__kernel void convolve (
          int nxp,
          int nyp,
          __global   float * I,
          __global   float * S,
          __constant float * F,
          __local    float * tiles )
{
   int k_lt, k_rt, k_up, k_dn;
   int4 halo;

   // pointers for interior and shifted regions
   float iI;

   // scalar quantities

   // these will likely be needed by all kernels
   const int2 t_size = {get_local_size(0) + 2*NPAD, get_local_size(1) + 2*NPAD};

   const int k   = KX + KY*NX;
   const int kex = (KX + NPAD) + (KY + NPAD)*(NX + 2*NPAD);
   
   // where will this come from
   const int kl   = KXL + KYL*t_size.s0;
   const int klex = (KXL + NPAD) + (KYL + NPAD)*t_size.s0;

   halo = (int4) (NPAD,NPAD,NPAD,NPAD);

   // allocate memory
   //
   
#if USE_LOCAL_MEM
   // explicit temporaries
   //
   __local float * I_tile = TILE_OFFSET(tiles, 0, t_size);

   // transfer data first
   //
   iI = transfer_halo(I, halo, I_tile);

   const int lStride = get_local_size(0) + halo.s0 + halo.s1;
   float val = 0.0;
   int f_idx = 0;

   barrier(CLK_LOCAL_MEM_FENCE);

   for (int j = 0; j < nyp; j++) {
      int i_idx = klex - halo.s0 - (j - halo.s1)*lStride;

      val += F[f_idx++] * I_tile[i_idx++];
      val += F[f_idx++] * I_tile[i_idx++];
      val += F[f_idx++] * I_tile[i_idx++];
#if NPAD > 1
      val += F[f_idx++] * I_tile[i_idx++];
      val += F[f_idx++] * I_tile[i_idx++];
#endif
#if NPAD > 2
      val += F[f_idx++] * I_tile[i_idx++];
      val += F[f_idx++] * I_tile[i_idx++];
#endif
#if NPAD > 3
      val += F[f_idx++] * I_tile[i_idx++];
      val += F[f_idx++] * I_tile[i_idx++];
#endif
#if NPAD > 4
      val += F[f_idx++] * I_tile[i_idx++];
      val += F[f_idx++] * I_tile[i_idx++];
#endif
#if NPAD > 5
      val += F[f_idx++] * I_tile[i_idx++];
      val += F[f_idx++] * I_tile[i_idx++];
#endif
#if NPAD > 6
      val += F[f_idx++] * I_tile[i_idx++];
      val += F[f_idx++] * I_tile[i_idx++];
#endif
   }
#else
   const int gStride = get_global_size(0) + halo.s0 + halo.s1;
   float val = 0.0;
   int f_idx = 0;

   for (int j = 0; j < nyp; j++) {
      int i_idx = kex - halo.s0 - (j - halo.s1)*gStride;

      val += F[f_idx++] * I[i_idx++];
      val += F[f_idx++] * I[i_idx++];
      val += F[f_idx++] * I[i_idx++];
#if NPAD > 1
      val += F[f_idx++] * I[i_idx++];
      val += F[f_idx++] * I[i_idx++];
#endif
#if NPAD > 2
      val += F[f_idx++] * I[i_idx++];
      val += F[f_idx++] * I[i_idx++];
#endif
#if NPAD > 3
      val += F[f_idx++] * I[i_idx++];
      val += F[f_idx++] * I[i_idx++];
#endif
#if NPAD > 4
      val += F[f_idx++] * I[i_idx++];
      val += F[f_idx++] * I[i_idx++];
#endif
#if NPAD > 5
      val += F[f_idx++] * I[i_idx++];
      val += F[f_idx++] * I[i_idx++];
#endif
#if NPAD > 6
      val += F[f_idx++] * I[i_idx++];
      val += F[f_idx++] * I[i_idx++];
#endif
   }
#endif

   S[k] = val;
}
