/**
 * kernel to test boundaries and act as a template for later expansion
 *
 */

__kernel void Prolongate_3D (
    int N, int M, int L,
    __global float * V1h, __global float * V2h )
{
   const unsigned int x = get_global_id(0);
   const unsigned int y = get_global_id(1);
   const unsigned int z = get_global_id(2);
   const unsigned int xx = 2 * x;
   const unsigned int yy = 2 * y;
   const unsigned int zz = 2 * z;

   if (x == 0 || y == 0 || z == 0)
      return;
   if (xx >= N-1 || y >= M-1 || z >= L-1)
      return;

   unsigned int n = N / 2; // SHOULD THIS BE N/2 - 1 ???

   const unsigned int x0y0z0 = x + y * n + z * n * M;
   const unsigned int x1y0z0 = x0y0z0 + 1; // 
   const unsigned int x0y1z0 = x0y0z0 + n;
   const unsigned int x1y1z0 = x0y1z0 + 1; //
   const unsigned int x0y0z1 = x0y0z0 + n*M;
   const unsigned int x1y0z1 = x0y0z1 + 1; //
   const unsigned int x0y1z1 = x0y0z0 + n + n*M;
   const unsigned int x1y1z1 = x0y1z1 + 1; //

   const unsigned int xx0yy0zz0 = xx + yy * n + zz * n * M;
   const unsigned int xx1yy0zz0 = xx0yy0zz0 + 1;
   const unsigned int xx0yy1zz0 = xx0yy0zz0 + n;
   const unsigned int xx0yy0zz1 = xx0yy0zz0 + n * M;
   const unsigned int xx1yy0zz1 = xx0yy0zz0 + 1 + n*M;
   const unsigned int xx1yy1zz0 = xx0yy0zz0 + 1 + n;
   const unsigned int xx0yy1zz1 = xx0yy0zz0 + n + n*M;
   const unsigned int xx1yy1zz1 = xx0yy0zz0 + 1 + n + n*M;

   V1h[xx0yy0zz0] = V2h[x0y0z0];
   V1h[xx1yy0zz0] = .5*(V2h[x0y0z0] + V2h[x1y0z0]);
   V1h[xx0yy1zz0] = .5*(V2h[x0y0z0] + V2h[x0y1z0]);
   V1h[xx0yy0zz1] = .5*(V2h[x0y0z0] + V2h[x0y0z1]);

   V1h[xx1yy1zz0] = .25*(V2h[x0y0z0] + V2h[x1y0z0] + V2h[x0y1z0] + V2h[x1y1z0]);
   V1h[xx1yy0zz1] = .25*(V2h[x0y0z0] + V2h[x1y0z0] + V2h[x0y0z1] + V2h[x1y0z1]);
   V1h[xx0yy1zz1] = .25*(V2h[x0y0z0] + V2h[x0y1z0] + V2h[x0y0z1] + V2h[x0y1z1]);
   V1h[xx1yy1zz1] = .125*(V2h[x0y0z0]
                        + V2h[x1y0z0] + V2h[x0y1z0] + V2h[x0y0z1]
                        + V2h[x1y1z0] + V2h[x1y1z0] + V2h[x0y1z1]
                        + V2h[x1y1z1]);

   return;
}