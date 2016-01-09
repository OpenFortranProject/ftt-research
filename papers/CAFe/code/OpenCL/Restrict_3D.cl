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

   if (x < 2 || y < 2 || z < 2)
      return;
   if (x > N || y > M || z > L)
      return;

   const unsigned int sx = 1;
   const unsigned int sy = N + 3;
   const unsigned int sz = sy * (M+3);
   const unsigned int x0y0z0 = x + y * sy + z * sy;

   const unsigned int x0y1z0 = x0y0z0 + sy;
   const unsigned int x0y_1z0 = x0y0z0 - sy;
   const unsigned int x0y1z_1 = x0y0z0 + sy - sz;
   const unsigned int x0y_1z1 = x0y0z0 - sy + sz;
   const unsigned int x0y0z_1 = x0y0z0 - sz;
   const unsigned int x0y0z1 = x0y0z0 + sz;
   const unsigned int x0y1z1 = x0y0z0 + sy + sz;
   const unsigned int x0y_1z_1 = x0y0z0 - sy - sz;

   V2h[x0y0z0] = .001 * (
                 .125*V1h[x0y1z1  -sx] + .25*V1h[x0y1z1  ] + .125*V1h[x0y1z1  +sx]
               +  .25*V1h[x0y0z1  -sx] +  .5*V1h[x0y0z1  ] +  .25*V1h[x0y0z1  +sx]
               + .125*V1h[x0y_1z1 -sx] + .25*V1h[x0y_1z1 ] + .125*V1h[x0y_1z1 +sx]
               +  .25*V1h[x0y1z0  -sx] +  .5*V1h[x0y1z0  ] +  .25*V1h[x0y1z0  +sx]
               +   .5*V1h[x0y0z0  -sx] +     V1h[x0y0z0  ] +   .5*V1h[x0y0z0  +sx]
               +  .25*V1h[x0y_1z0 -sx] +  .5*V1h[x0y_1z0 ] +  .25*V1h[x0y_1z0 +sx]
               + .125*V1h[x0y1z_1 -sx] + .25*V1h[x0y1z_1 ] + .125*V1h[x0y1z_1 +sx]
               +  .25*V1h[x0y0z_1 -sx] +  .5*V1h[x0y0z_1 ] +  .25*V1h[x0y0z_1 +sx]
               + .125*V1h[x0y_1z_1-sx] + .25*V1h[x0y_1z_1] + .125*V1h[x0y_1z_1+sx]
                 );

   return;
}