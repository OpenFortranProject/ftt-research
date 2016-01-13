/**
 * kernel to test boundaries and act as a template for later expansion
 *
 */

__kernel void Relax_3D (
    int N, int M, int L,
    __global float * A, __global float * Tmp )
{
   const unsigned int x = get_global_id(0) + 2; // x=2 is first interior point
   const unsigned int y = get_global_id(1) + 2; // y=2 is first interior point
   const unsigned int z = get_global_id(2) + 2; // z=2 is first interior point

   if (x < 2 || y < 2 || z < 2)
      return;
   if (x > N || y > M || z > L)
      return;

   const unsigned int sx = 1;          // stride in x
   const unsigned int sy = sx*(N+3);   // stride in y
   const unsigned int sz = sy*(M+3);   // stride in z
   const float w = 2.0 / 3.0;

   unsigned int k0 = x + y * sy + z * sz;

   Tmp[k0] = (1.0 - w) * A[k0]
             + (1.0/6.0)*w*(
                            A[k0-sx] + A[k0+sx] 
                          + A[k0-sy] + A[k0+sy]
                          + A[k0-sz] + A[k0+sz]
                           );

   barrier(CLK_GLOBAL_MEM_FENCE);

   A[k0] = Tmp[k0];

   return;
}