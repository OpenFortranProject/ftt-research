/**
 * kernel to tranfer boundary
 *
 */

#define INDEX

__kernel void GetBoundary_3D (
    int N, int M, int L,
    __global float * A, __global float * Buf )
{
   const unsigned int x = get_global_id(0);
   const unsigned int y = get_global_id(1);
   unsigned int z = get_global_id(2);
   unsigned int xx, yy, zz, k0, b0;
   const unsigned int sx = 1;
   const unsigned int sy = sx*((M-1)*(L-1)+(N-1)*(L-1)+(N-1)*(M-1));
   const unsigned int sz = 1;//sy*1;
   const unsigned int sxx = 1;
   const unsigned int syy = sxx*(N+3);
   const unsigned int szz = syy*(M+3);
   const unsigned int n = N - 1;
   const unsigned int m = M - 1;
   const unsigned int l = L - 1;

   if (x < m*l) {
     zz = x / l + 2;
     yy = x % m + 2;
     if (y == 0)
       xx = 1;
     else {
       xx = L+1;
       z = 1;
     }

     b0 = x + y*(N-1)*(M-1);
     k0 = (xx) + (yy)*(syy) + (zz)*(szz);

     if (Buf[b0] != 0)
       Buf[b0] = -999;
     else {
       // Tranfer cpu calculation
       if (xx = 1)
	 A[k0+sxx] = Buf[b0];
       else
	 A[k0-sxx] = Buf[b0];
       // Tranfer gpu calculation
       Buf[b0] = A[k0];
#ifdef INDEX
	 Buf[b0] = 9000+xx*100+yy*10+zz;
#endif
     }
     return;
   }
   else if (x < m*l+n*l){
     int start = m*l;
     zz = x / l - 1;
     xx = x % m + 2;
     if (y == 0)
       yy = 1;
     else {
       yy = L+1;
       z = 1;
     }

     b0 = x + y*(N-1)*(M-1) + start;
     k0 = (xx) + (yy)*(syy) + (zz)*(szz);

     if (Buf[b0] != 0)
       Buf[b0] = -999;
     else {
       // Tranfer cpu calculation
       if (yy = 1)
	 A[k0+syy] = Buf[b0];
       else
	 A[k0-syy] = Buf[b0];
       // Tranfer gpu calculation
       Buf[b0] = A[k0];
#ifdef INDEX
	 Buf[b0] = 9000+xx*100+yy*10+zz;
#endif
     }
     return;
   }
   else {
     int start = m*l+n*l;
     yy = x / l - 4;
     xx = x % m + 2;
     if (y == 0)
       zz = 1;
     else {
       zz = L+1;
       z = 1;
     }

     b0 = x + y*(N-1)*(M-1) + start;
     k0 = (xx) + (yy)*(syy) + (zz)*(szz);

     if (Buf[b0] != 0)
       Buf[b0] = -999;
     else {
       // Tranfer cpu calculation
       if (zz = 1)
	 A[k0+szz] = Buf[b0];
       else
	 A[k0-szz] = Buf[b0];
       // Tranfer gpu calculation
       Buf[b0] = A[k0];
#ifdef INDEX
	 Buf[b0] = 9000+xx*100+yy*10+zz;
#endif
     }
     return;
   }

   return;
}
