/**
 * kernel to test boundaries and act as a template for later expansion
 *
 */

__kernel void Prolongate_3D (
    int N, int M, int L,
    __global float * V1h, __global float * V2h )
{
   const unsigned int X = get_global_id(0)+2;
   const unsigned int Y = get_global_id(1)+2;
   const unsigned int Z = get_global_id(2)+2;

   if (X <= 1 || Y <= 1 || Z <= 1)
      return;
   if (X > N || Y > M || Z > L)
      return;

   const unsigned int SX = 1;
   const unsigned int SY = SX*(N+3);
   const unsigned int SZ = SY*(M+3);
   const unsigned int K0 = X + Y * SY + Z * SZ;
   const unsigned int x = (X+1)/2;
   const unsigned int y = (Y+1)/2;
   const unsigned int z = (Z+1)/2;
   const unsigned int n = N / 2;
   const unsigned int m = M / 2;
   const unsigned int l = L / 2;
   const unsigned int sx = 1;
   const unsigned int sy = sx*(n+3);
   const unsigned int sz = sy*(m+3);
   const unsigned int k0 = x + y * sy + z * sz;

   // Check what algorithm to use
   if (X == Y && Y == Z && X%2!=0) {
      V1h[K0] = V2h[k0];
      V1h[K0] = 1;
      }
   else if (X%2==0 && Y%2==0 && Z%2==0) {
      V1h[K0] = .125*(
               V2h[k0+sx+sy+sz]+V2h[k0-sx-sy-sz]
            +  V2h[k0+sx+sy-sz]+V2h[k0+sx-sy+sz]+V2h[k0-sx+sy+sz]
            +  V2h[k0-sx-sy+sz]+V2h[k0-sx+sy-sz]+V2h[k0+sx-sy-sz]);
      V1h[K0] = .125;
     }
   else if (X%2==0 && Y==Z) {
      V1h[K0] = .5*(V2h[k0+sx]+V2h[k0-sx]);
      V1h[K0] = .5;
     }
   else if (Y%2==0 && X==Z) {
      V1h[K0] = .5*(V2h[k0+sy]+V2h[k0-sy]);
      V1h[K0] = .5;
     }
   else if (Z%2==0 && X==Y) {
      V1h[K0] = .5*(V2h[k0+sz]+V2h[k0-sz]);
      V1h[K0] = .5;
     }
   // SHOULD DOUBLE CHECK THAT THE .25 INDEXING IS OCCURRING CORRECTLY
   else if (X%2==0 && Y%2==0) {
      V1h[K0] = .25*(V2h[k0+sx+sy]+V2h[k0+sx-sy] + V2h[k0-sx+sy]+V2h[k0-sx-sy]);
      V1h[K0] = .25;
      }
   else if (Y%2==0 && Z%2==0) {
      V1h[K0] = .25*(V2h[k0+sy+sz]+V2h[k0+sy-sz] + V2h[k0-sy+sz]+V2h[k0-sy-sz]);
      V1h[K0] = .25;
      }
   else if (X%2==0 && Z%2==0) {
      V1h[K0] = .25*(V2h[k0+sx+sz]+V2h[k0+sx-sz] + V2h[k0-sx+sz]+V2h[k0-sx-sz]);
      V1h[K0] = .25;
      }

   return;

   V1h[K0] = Z+100*Y+10000*X;
   return;
}