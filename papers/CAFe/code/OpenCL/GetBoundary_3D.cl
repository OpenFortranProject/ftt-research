/**
 * kernel to tranfer boundary
 *
 */

__kernel void GetBoundary_3D (
    int N, int M, int L,
    __global float * A, __global float * Buf )
{
   const unsigned int x = get_global_id(0);
   const unsigned int y = get_global_id(1);
   const unsigned int z = get_global_id(2);
   const unsigned int sx = 1;
   const unsigned int sy = sx*(N+3);
   const unsigned int sz = sy*(M+3);
   const unsigned int sb = 5;
   unsigned int k0, b0;

   b0 = x + y * sb + z * sb * sb;

   if (x < 2 || y < 2 || z < 2) {
     k0 = x + y * sy + z * sz;
     Buf[b0] = A[k0];
   }
   else if (x == 2 && y == 2 && z == 2)
     Buf[b0] = 0;
   else if (x < 5 || y < 5 || z < 5) {
     const unsigned int newx = x+N-2;
     const unsigned int newy = y+M-2;
     const unsigned int newz = z+L-2;
     k0 = newx + newy * sy + newz * sz;
     Buf[b0] = A[k0];
   }

   return;
}
