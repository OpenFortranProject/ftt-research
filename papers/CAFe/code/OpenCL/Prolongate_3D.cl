/**
 * kernel to test boundaries and act as a template for later expansion
 *
 */

__kernel void Prolongate_3D (
    int N, int M, int L,
    __global float * V1h, __global float * V2h )
{
   const unsigned int x = get_global_id(0)+2;
   const unsigned int y = get_global_id(1)+2;
   const unsigned int z = get_global_id(2)+2;
   const unsigned int X = x*2-1;
   const unsigned int Y = y*2-1;
   const unsigned int Z = z*2-1;

/*   if (x == 0 || y == 0 || z == 0)
      return;
   if (X > N || Y > M || Z > L)
      return;*/

   const unsigned int n = N / 2;
   const unsigned int m = M / 2;
   const unsigned int l = L / 2;
   const unsigned int sx = 1;
   const unsigned int sy = sx*(n+3);
   const unsigned int sz = sy*(m+3);
   const unsigned int k0 = x + y * sy + z * sz;

   const unsigned int SX = 1;
   const unsigned int SY = SX*(N+3);
   const unsigned int SZ = SY*(M+3);
   const unsigned int K0 = X + Y * SY + Z * SZ;

   //V1h[xx0yy0zz0] = V2h[x0y0z0];
   //V1h[xx0yy0zz0] = -666;
   //V1h[x0y0z0] = -666;
   V1h[K0] = V2h[k0];
   //V1h[K0] = K0;

   V1h[K0-SX] = .5*(V2h[k0] + V2h[k0-sx]);
   V1h[K0+SX] = .5*(V2h[k0] + V2h[k0+sx]);
   V1h[K0-SY] = .5*(V2h[k0] + V2h[k0-sy]);
   V1h[K0+SY] = .5*(V2h[k0] + V2h[k0+sy]);
   V1h[K0-SZ] = .5*(V2h[k0] + V2h[k0-sz]);
   V1h[K0+SZ] = .5*(V2h[k0] + V2h[k0+sz]);

   return;
   /*
   V1h[xx1yy0zz0] = .5*(V2h[x0y0z0] + V2h[x1y0z0]);
   V1h[xx0yy1zz0] = .5*(V2h[x0y0z0] + V2h[x0y1z0]);
   V1h[xx0yy0zz1] = .5*(V2h[x0y0z0] + V2h[x0y0z1]);

   V1h[xx1yy1zz0] = .25*(V2h[x0y0z0] + V2h[x1y0z0] + V2h[x0y1z0] + V2h[x1y1z0]);
   V1h[xx1yy0zz1] = .25*(V2h[x0y0z0] + V2h[x1y0z0] + V2h[x0y0z1] + V2h[x1y0z1]);
   V1h[xx0yy1zz1] = .25*(V2h[x0y0z0] + V2h[x0y1z0] + V2h[x0y0z1] + V2h[x0y1z1]);
   V1h[xx1yy1zz1] = .125*(V2h[x0y0z0]
                        + V2h[x1y0z0] + V2h[x0y1z0] + V2h[x0y0z1]
                        + V2h[x1y1z0] + V2h[x1y1z0] + V2h[x0y1z1]
                        + V2h[x1y1z1]);*/

   return;
}