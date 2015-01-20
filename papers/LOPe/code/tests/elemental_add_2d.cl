#include "halos_2d.h"

#define COPY_HALOS

/**
 * simple kernel that computes the sum of array elements
 */
__kernel void elemental_add_2d (
    __global float * A,
    __global float * B,
    __global float * C, __global float * C_H_ )
{
   if (K1_ < N1_ && K2_ < N2_) {

#ifdef COPY_HALOS
      // copy incoming halo regions (to halo region of array)
      // ----------------------------------------------------------------------------------------------
     if (K2_ < HALO2(C,L)) {
        C[IDX2(C,0,0)] = C_H_[K1_];
     }
     if (K2_ >= N2_ - HALO2(C,R)) {
        C[IDX2(C,0,0)] = C_H_[K1_+N1_];
     }

     if (K1_ < HALO1(C,L)) {
        C[IDX2(C,0,0)] = C_H_[2*N1_+K2_];
     }
     if (K1_ >= N1_ - HALO1(C,R)) {
        C[IDX2(C,0,0)] = C_H_[2*N1_+N2_+K2_];
     }
#endif


     // run algorithm
     // ----------------------------------------------------------------------------------------------

      C[IDX2(C,0,0)] = A[IDX2(A,0,0)] + B[IDX2(B,0,0)];


#ifdef COPY_HALOS
      // copy outgoing halo regions
      // ----------------------------------------------------------------------------------------------
     if (K2_ < HALO2(C,L)) {
        C_H_[K1_] = C[IDX2(C,0,0)];
     }
     if (K2_ >= N2_ - HALO2(C,R)) {
        C_H_[K1_+N1_] = C[IDX2(C,0,0)];
     }

     if (K1_ < HALO1(C,L)) {
        C_H_[2*N1_+K2_] = C[IDX2(C,0,0)];
     }
     if (K1_ >= N1_ - HALO1(C,R)) {
        C_H_[2*N1_+N2_+K2_] = C[IDX2(C,0,0)];
     }
#endif

   }

}
