/**
 * kernel to test boundaries and act as a template for later expansion
 *
 */

__kernel void Restrict_3D (
    int N, int M, int L,
    __global float * V1h, __global float * V2h )
{
   const unsigned int x = get_global_id(0);
   const unsigned int y = get_global_id(1);
   const unsigned int z = get_global_id(2);
   unsigned int n = N / 2; // SHOULD THIS BE N / 2 - 1 ???

   const unsigned int x0y0z0 = x + y * n + z * n * M;
   const unsigned int x_1y0z0 = x0y0z0 - 1; 
   const unsigned int x1y0z0 = x0y0z0 + 1; // 
   const unsigned int x0y1z0 = x0y0z0 + n;
   const unsigned int x_1y1z0 = x0y1z0 - 1; 
   const unsigned int x1y1z0 = x0y1z0 + 1; //
   const unsigned int x0y_1z0 = x0y0z0 - n;
   const unsigned int x_1y_1z0 = x0y_1z0 - 1; 
   const unsigned int x1y_1z0 = x0y_1z0 + 1; //
   const unsigned int x0y1z_1 = x0y0z0 + n - n*M;
   const unsigned int x_1y1z_1 = x0y1z_1 - 1; 
   const unsigned int x1y1z_1 = x0y1z_1 + 1; //
   const unsigned int x0y_1z1 = x0y0z0 - n + n*M;
   const unsigned int x_1y_1z1 = x0y_1z1 - 1; 
   const unsigned int x1y_1z1 = x0y_1z1 + 1; //
   const unsigned int x0y0z_1 = x0y0z0 - n*M;
   const unsigned int x_1y0z_1 = x0y0z_1 - 1; 
   const unsigned int x1y0z_1 = x0y0z_1 + 1; //
   const unsigned int x0y0z1 = x0y0z0 + n*M;
   const unsigned int x_1y0z1 = x0y0z1 - 1; 
   const unsigned int x1y0z1 = x0y0z1 + 1; //
   const unsigned int x0y1z1 = x0y0z0 + n + n*M;
   const unsigned int x_1y1z1 = x0y1z1 - 1; 
   const unsigned int x1y1z1 = x0y1z1 + 1; //
   const unsigned int x0y_1z_1 = x0y0z0 - n - n*M;
   const unsigned int x_1y_1z_1 = x0y_1z_1 - 1; 
   const unsigned int x1y_1z_1 = x0y_1z_1 + 1; //

   
   if (x == 0 || y == 0 || z == 0)
      return;
   if (x >= N-1 || y >= M-1 || z >= L-1)
      return;

   V2h[x0y0z0] = .1 * (
                 .125*V1h[x_1y1z1]   + .25*V1h[x0y1z1]   + .125*V1h[x1y1z1]
               +  .25*V1h[x_1y0z1]   +  .5*V1h[x0y0z1]   +  .25*V1h[x1y0z1]
               + .125*V1h[x_1y_1z1]  + .25*V1h[x0y_1z1]  + .125*V1h[x1y_1z1]
               +  .25*V1h[x_1y1z0]   +  .5*V1h[x0y1z0]   +  .25*V1h[x1y1z0]
               +   .5*V1h[x_1y0z0]   +     V1h[x0y0z0]   +   .5*V1h[x1y0z0]
               +  .25*V1h[x_1y_1z0]  +  .5*V1h[x0y_1z0]  +  .25*V1h[x1y_1z0]
               + .125*V1h[x_1y1z_1]  + .25*V1h[x0y1z_1]  + .125*V1h[x1y1z_1]
               +  .25*V1h[x_1y0z_1]  +  .5*V1h[x0y0z_1]  +  .25*V1h[x1y0z_1]
               + .125*V1h[x_1y_1z_1] + .25*V1h[x0y_1z_1] + .125*V1h[x1y_1z_1]
                 );

   return;
}