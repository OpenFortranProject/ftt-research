#define DO_COMPUTATION

#define NPAD 1

#define KXL get_local_id(0)
#define KYL get_local_id(1)

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
 * time advance step for shallow water
 */
__kernel void wave_advance (
          __global float * H,
          __global float * U,
          __global float * V,
          float dx,
          float dt,
          __local float * tiles )
{
   int k_lt, k_rt, k_up, k_dn;
   int4 halo, face_lt, face_rt, face_up, face_dn;

   // pointers for interior and shifted regions
   float iH, iU, iV;

   // scalar quantities
   float cs, gs, dtdx, dtdx2;

   // these will likely be needed by all kernels
   const int2 t_size = {get_local_size(0) + 2*NPAD, get_local_size(1) + 2*NPAD};
   const int k = (get_global_id(0) + NPAD)
               + (get_global_id(1) + NPAD) * (get_global_size(0) + 2*NPAD);
   
   // where will this come from
   int kl = KXL + KYL*t_size.s0;

   halo = (int4) (NPAD,NPAD,NPAD,NPAD);

   // allocate memory
   //
   
   // explicit temporaries
   __local float * Hx, * Hy, * Ux, * Uy, * Vx, * Vy;

   Hx = TILE_OFFSET(tiles, 0, t_size);
   Hy = TILE_OFFSET(tiles, 1, t_size);
   Ux = TILE_OFFSET(tiles, 2, t_size);
   Uy = TILE_OFFSET(tiles, 3, t_size);
   Vx = TILE_OFFSET(tiles, 4, t_size);
   Vy = TILE_OFFSET(tiles, 5, t_size);

   // transfer data first
   //

   __local float * H_tile = TILE_OFFSET(tiles, 6, t_size);
   iH = transfer_halo(H, halo, H_tile);

   __local float * U_tile = TILE_OFFSET(tiles, 7, t_size);
   iU = transfer_halo(U, halo, U_tile);

   __local float * V_tile = TILE_OFFSET(tiles, 8, t_size);
   iV = transfer_halo(V, halo, V_tile);

   barrier(CLK_LOCAL_MEM_FENCE);

#ifdef DO_COMPUTATION

   // first half step
   //

   gs = 0.5 * 9.8;
   dtdx = dt/dx;
   dtdx2 = 0.5 * dt/dx;

   face_lt = (int4) (0,1,1,1);
   face_rt = (int4) (1,0,1,1);
   face_dn = (int4) (1,1,0,1);
   face_up = (int4) (1,1,1,0);

   k_lt = region_idx(face_lt, t_size);
   k_rt = region_idx(face_rt, t_size);
   k_dn = region_idx(face_dn, t_size);
   k_up = region_idx(face_up, t_size);

   // height
   Hx[kl] =     0.5 * ( H_tile[k_lt] + H_tile[k_rt] )
            + dtdx2 * ( U_tile[k_lt] - U_tile[k_rt] );

   // x momentum
   Ux[kl] =     0.5 * ( H_tile[k_lt] + U_tile[k_rt] )
            + dtdx2 * ( U_tile[k_lt]*U_tile[k_lt] / H_tile[k_lt] + gs*H_tile[k_lt]*H_tile[k_lt] )
            - dtdx2 * ( U_tile[k_rt]*U_tile[k_rt] / H_tile[k_rt] + gs*H_tile[k_rt]*H_tile[k_rt] );

   // x momentum
   Vx[kl] =     0.5 * ( V_tile[k_lt] + V_tile[k_rt] )
            + dtdx2 * ( U_tile[k_lt] * V_tile[k_lt] / H_tile[k_lt] )
            - dtdx2 * ( U_tile[k_rt] * V_tile[k_rt] / H_tile[k_rt] );

   // height
   Hy[kl] =     0.5 * ( H_tile[k_dn] + H_tile[k_up] )
            + dtdx2 * ( U_tile[k_dn] - U_tile[k_up] );

   // x momentum
   Uy[kl] =     0.5 * ( U_tile[k_dn] + U_tile[k_up] )
            + dtdx2 * ( U_tile[k_dn] * V_tile[k_dn] / H_tile[k_dn] )
            - dtdx2 * ( U_tile[k_up] * V_tile[k_up] / H_tile[k_up] );

   // x momentum
   Vy[kl] =     0.5 * ( V_tile[k_dn] + V_tile[k_up] )
            + dtdx2 * ( V_tile[k_dn]*V_tile[k_dn] / H_tile[k_dn] + gs*H_tile[k_dn]*H_tile[k_dn] )
            - dtdx2 * ( V_tile[k_up]*V_tile[k_up] / H_tile[k_up] + gs*H_tile[k_up]*H_tile[k_up] );

   if (KXL < 1) {
      kl   += get_local_size(0);
      k_lt += get_local_size(0);
      k_rt += get_local_size(0);

      // height
      Hx[kl] =     0.5 * ( H_tile[k_lt] + H_tile[k_rt] )
               + dtdx2 * ( U_tile[k_lt] - U_tile[k_rt] );

      // x momentum
      Ux[kl] =     0.5 * ( H_tile[k_lt] + U_tile[k_rt] )
               + dtdx2 * ( U_tile[k_lt]*U_tile[k_lt] / H_tile[k_lt] + gs*H_tile[k_lt]*H_tile[k_lt] )
               - dtdx2 * ( U_tile[k_rt]*U_tile[k_rt] / H_tile[k_rt] + gs*H_tile[k_rt]*H_tile[k_rt] );

      // x momentum
      Vx[kl] =     0.5 * ( V_tile[k_lt] + V_tile[k_rt] )
               + dtdx2 * ( U_tile[k_lt] * V_tile[k_lt] / H_tile[k_lt] )
               - dtdx2 * ( U_tile[k_rt] * V_tile[k_rt] / H_tile[k_rt] );

      kl   -= get_local_size(0);
      k_lt -= get_local_size(0);
      k_rt -= get_local_size(0);
   }

   // shift down to pick up extra row for face
   if (KYL < 1) {
      kl   += get_local_size(1) * t_size.s0;
      k_lt += get_local_size(1) * t_size.s0;
      k_rt += get_local_size(1) * t_size.s0;

      // height
      Hy[kl] =     0.5 * ( H_tile[k_dn] + H_tile[k_up] )
               + dtdx2 * ( U_tile[k_dn] - U_tile[k_up] );

      // x momentum
      Uy[kl] =     0.5 * ( U_tile[k_dn] + U_tile[k_up] )
               + dtdx2 * ( U_tile[k_dn] * V_tile[k_dn] / H_tile[k_dn] )
               - dtdx2 * ( U_tile[k_up] * V_tile[k_up] / H_tile[k_up] );

      // x momentum
      Vy[kl] =     0.5 * ( V_tile[k_dn] + V_tile[k_up] )
               + dtdx2 * ( V_tile[k_dn]*V_tile[k_dn] / H_tile[k_dn] + gs*H_tile[k_dn]*H_tile[k_dn] )
               - dtdx2 * ( V_tile[k_up]*V_tile[k_up] / H_tile[k_up] + gs*H_tile[k_up]*H_tile[k_up] );

      kl   -= get_local_size(1) * t_size.s0;
      k_lt -= get_local_size(1) * t_size.s0;
      k_rt -= get_local_size(1) * t_size.s0;
   }

   // storing to local memory so need to sync
   barrier(CLK_LOCAL_MEM_FENCE);

   // second half step
   //

   face_lt = (int4) (0,1,0,0);
   face_rt = (int4) (1,0,0,0);
   face_dn = (int4) (0,0,0,1);
   face_up = (int4) (0,0,1,0);

   k_lt = region_idx(face_lt, t_size);
   k_rt = region_idx(face_rt, t_size);
   k_dn = region_idx(face_dn, t_size);
   k_up = region_idx(face_up, t_size);

   // height
   H[k] = iH + dtdx * ( Ux[k_lt] - Ux[k_rt] )
             + dtdx * ( Vy[k_dn] - Vy[k_up] );

   // x momentum
   U[k] = iU + dtdx * ( Ux[k_lt]*Ux[k_lt] / Hx[k_lt] + gs*Hx[k_lt]*Hx[k_lt] )
             - dtdx * ( Ux[k_rt]*Ux[k_rt] / Hx[k_rt] + gs*Hx[k_rt]*Hx[k_rt] )
             + dtdx * ( Uy[k_dn] * Vy[k_dn] / Hy[k_dn] )
             - dtdx * ( Uy[k_up] * Vy[k_up] / Hy[k_up] );

   // y momentum
   V[k] = iV + dtdx * ( Ux[k_dn] * Vx[k_dn] / Hx[k_dn] )
             - dtdx * ( Ux[k_up] * Vx[k_up] / Hx[k_up] )
             + dtdx * ( Vy[k_lt]*Vy[k_lt] / Hy[k_lt] + gs*Hy[k_lt]*Hy[k_lt] )
             - dtdx * ( Vy[k_rt]*Vy[k_rt] / Hy[k_rt] + gs*Hy[k_rt]*Hy[k_rt] );

#else

   H[k] = U_tile[kl];
   U[k] = V_tile[kl];
   V[k] = H_tile[kl];

#endif

}
