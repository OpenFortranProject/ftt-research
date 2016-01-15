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

   if (X < 2 || Y < 2 || Z < 2)
      return;
   if (X > N || Y > M || Z > L)
      return;

   const unsigned int SX = 1;
   const unsigned int SY = SX*(N+3);
   const unsigned int SZ = SY*(M+3);
   const unsigned int K0 = X + Y * SY + Z * SZ;
   const unsigned int sx = 1;
   const unsigned int sy = sx*((N/2)+3);
   const unsigned int sz = sy*((M/2)+3);
   /* Indexing for k0:   x=(X+1)/2; y=(Y+1)/2;       z=(Z+1)/2; */
   const unsigned int k0 = (X+1)/2 + ((Y+1)/2) * sy + ((Z+1)/2) * sz;

   // Based on index the proper algorithm is chosen
   if (X == Y && Y == Z && X%2!=0)
      V1h[K0] = V2h[k0];
   else if (X%2==0 && Y%2==0 && Z%2==0)
      V1h[K0] = .125*(
               V2h[k0+sx+sy+sz]+V2h[k0-sx-sy-sz]
            +  V2h[k0+sx+sy-sz]+V2h[k0+sx-sy+sz]+V2h[k0-sx+sy+sz]
            +  V2h[k0-sx-sy+sz]+V2h[k0-sx+sy-sz]+V2h[k0+sx-sy-sz]);
   else if (X%2==0 && Y==Z)
      V1h[K0] = .5*(V2h[k0+sx] + V2h[k0-sx]);
   else if (Y%2==0 && X==Z)
      V1h[K0] = .5*(V2h[k0+sy] + V2h[k0-sy]);
   else if (Z%2==0 && X==Y)
      V1h[K0] = .5*(V2h[k0+sz] + V2h[k0-sz]);
   else if (X%2==0 && Y%2==0)
      V1h[K0] = .25*(V2h[k0+sx+sy] + V2h[k0+sx-sy] + V2h[k0-sx+sy] + V2h[k0-sx-sy]);
   else if (Y%2==0 && Z%2==0)
      V1h[K0] = .25*(V2h[k0+sy+sz] + V2h[k0+sy-sz] + V2h[k0-sy+sz] + V2h[k0-sy-sz]);
   else if (X%2==0 && Z%2==0)
      V1h[K0] = .25*(V2h[k0+sx+sz] + V2h[k0+sx-sz] + V2h[k0-sx+sz] + V2h[k0-sx-sz]);

   return;
}