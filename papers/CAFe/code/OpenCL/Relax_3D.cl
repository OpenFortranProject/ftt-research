/**
 * kernel to test boundaries and act as a template for later expansion
 *
 */

__kernel void Relax_3D (
    int N, int M, int L,
    __global float * A, __global float * Tmp )
{
   const unsigned int x = get_global_id(0);    // 0 is -1
   const unsigned int y = get_global_id(1);
   const unsigned int z = get_global_id(2);

   if (x < 2 || y < 2 || z < 2)
      return;
   if (x >= N+1 || y >= M+1 || z >= L+1)
      return;

   const unsigned int n = N ; // SHOULD THIS BE N / 2 - 1 ???
   const unsigned int sx = 1;
   const unsigned int sy = N + 3;
   const unsigned int sz = sy * (M+3);
   const float w = 2.0 / 3.0;

   const unsigned int x0y0z0 = x + y * sy + z * sz;
   const unsigned int x_1y0z0 = x0y0z0 - sx; 
   const unsigned int x1y0z0 = x0y0z0 + sx;
   const unsigned int x0y_1z0 = x0y0z0 - sy;
   const unsigned int x0y1z0 = x0y0z0 + sy;
   const unsigned int x0y0z_1 = x0y0z0 - sz;
   const unsigned int x0y0z1 = x0y0z0 + sz;

   Tmp[x0y0z0] = (1.0 - w) * A[x0y0z0]
               + (1.0/6.0)*w*(
                 A[x0y0z0 -1*sx] + A[x1y0z0 + 1*sx] 
               + A[x0y_1z0] + A[x0y1z0]
               + A[x0y0z_1] + A[x0y0z1] // SHOULD THIS BE ADDED?
                 );
   barrier(CLK_GLOBAL_MEM_FENCE);

   A[x0y0z0] = Tmp[x0y0z0];

   /*
   A[x0y0z0] = (1.0 - w) * Tmp[x0y0z0]
               + (1.0/6.0)*w*(
                 Tmp[x_1y0z0] + Tmp[x1y0z0] 
               + Tmp[x0y_1z0] + Tmp[x0y1z0]
               + Tmp[x0y0z_1] + Tmp[x0y0z1]
                 );
  */

   return;
}