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
#endif


     // run algorithm
     // ----------------------------------------------------------------------------------------------

      C[IDX2(C,0,0)] = A[IDX2(A,0,0)] + B[IDX2(B,0,0)];


      // copy outgoing halo regions
      // ----------------------------------------------------------------------------------------------

#ifdef COPY_HALOS
     if (K2_ < C_H2L_) {
        C_H_[K1_] = C[K1_];
     }
     if (K2_ >= N2_ - C_H2R_) {
        C_H_[K1_+N1_] = C[K1_ + (N2_-C_H2R_)*N1_];
     }

     if (K1_ < C_H1L_) {
        C_H_[2*N1_+K2_] = C[K1_ + K2_*N1_];
     }
     if (K1_ >= N1_ - C_H1R_) {
        C_H_[2*N1_+N2_+K2_] = C[K1_ + K2_*N1_];
     }
#endif

   }

}
