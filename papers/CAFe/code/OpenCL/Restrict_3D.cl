/**
 * kernel to test boundaries and act as a template for later expansion
 *
 */

__kernel void Restrict_3D (
    int N, int M, int L,
    __global float * V1h, __global float * V2h )
{
   const unsigned int x = get_global_id(0)+2;
   const unsigned int y = get_global_id(1)+2;
   const unsigned int z = get_global_id(2)+2;
   const unsigned int n = N / 2;
   const unsigned int m = M / 2;
   const unsigned int l = L / 2;

   if (x < 2 || y < 2 || z < 2)
      return;
   if (x > n || y > m || z > l)
      return;

   const int sx = 1;
   const int sy = sx*(n+3);
   const int sz = sy*(m+3);
   const unsigned int k0 = x + y * sy + z * sz;
   const int SX = 1;
   const int SY = SX*(N+3);
   const int SZ = SY*(M+3);
   const unsigned int K0 = x + y * SY + z * SZ;

   //V2h[k0] = x*10000+y*100+z + 9000000;
   //return;

/*   V2h[k0] = .001 * (
             .125*V1h[k0+sy+sz-sx] + .25*V1h[k0+sy+sz] + .125*V1h[k0+sy+sz+sx]   	       
           +  .25*V1h[k0   +sz-sx] +  .5*V1h[k0   +sz] +  .25*V1h[k0   +sz+sx]
           + .125*V1h[k0-sy+sz-sx] + .25*V1h[k0-sy+sz] + .125*V1h[k0-sy+sz+sx]
           +  .25*V1h[k0+sy   -sx] +  .5*V1h[k0+sy   ] +  .25*V1h[k0+sy   +sx]
           +   .5*V1h[k0      -sx] +     V1h[k0      ] +   .5*V1h[k0      +sx]
           +  .25*V1h[k0-sy   -sx] +  .5*V1h[k0-sy   ] +  .25*V1h[k0-sy   +sx]
           + .125*V1h[k0+sy-sz-sx] + .25*V1h[k0+sy-sz] + .125*V1h[k0+sy-sz+sx]
           +  .25*V1h[k0   -sz-sx] +  .5*V1h[k0   -sz] +  .25*V1h[k0   -sz+sx]
           + .125*V1h[k0-sy-sz-sx] + .25*V1h[k0-sy-sz] + .125*V1h[k0-sy-sz+sx]
   	     	    );*/

   V2h[k0] = .001 * (
             .125*V1h[K0+SY+SZ-SX] + .25*V1h[K0+SY+SZ] + .125*V1h[K0+SY+SZ+SX]   	       
           +  .25*V1h[K0   +SZ-SX] +  .5*V1h[K0   +SZ] +  .25*V1h[K0   +SZ+SX]
           + .125*V1h[K0-SY+SZ-SX] + .25*V1h[K0-SY+SZ] + .125*V1h[K0-SY+SZ+SX]
           +  .25*V1h[K0+SY   -SX] +  .5*V1h[K0+SY   ] +  .25*V1h[K0+SY   +SX]
           +   .5*V1h[K0      -SX] +     V1h[K0      ] +   .5*V1h[K0      +SX]
           +  .25*V1h[K0-SY   -SX] +  .5*V1h[K0-SY   ] +  .25*V1h[K0-SY   +SX]
           + .125*V1h[K0+SY-SZ-SX] + .25*V1h[K0+SY-SZ] + .125*V1h[K0+SY-SZ+SX]
           +  .25*V1h[K0   -SZ-SX] +  .5*V1h[K0   -SZ] +  .25*V1h[K0   -SZ+SX]
           + .125*V1h[K0-SY-SZ-SX] + .25*V1h[K0-SY-SZ] + .125*V1h[K0-SY-SZ+SX]
   	     	    );

   return;
}