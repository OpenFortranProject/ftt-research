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
   size_t hoff_, hsft_;

   if (K1_ < N1_ && K2_ < N2_) {

#ifdef COPY_HALOS
      // copy incoming halo regions (to halo region of array)
      // ----------------------------------------------------------------------------------------------

     hoff_ = 0;
     if (K1_ < HALO1(C,L)) {
        C[IDX2(C,-HALO1(C,L),0)] = C_H_[hoff_ + K1_ + K2_*HALO1(C,L)];
     }

     hoff_ += HALO1(C,L) * N2_;
     hsft_  = N1_ - HALO1(C,R);
     if (K1_ >= hsft_) {
        C[IDX2(C,HALO1(C,R),0)] = C_H_[hoff_ + (K1_ - hsft_) + K2_*HALO1(C,R)];
     }

     hoff_ += HALO1(C,R) * N2_;
     if (K2_ < HALO2(C,L)) {
        C[IDX2(C,0,-HALO2(C,L))] = C_H_[hoff_ + K1_ + K2_*N1_];
     }

     hoff_ += HALO2(C,L) * N1_;
     hsft_  = N2_ - HALO2(C,R);
     if (K2_ >= hsft_) {
        C[IDX2(C,0,HALO2(C,R))] = C_H_[hoff_ + K1_ + (K2_ - hsft_)*N1_];
     }

     barrier (CLK_GLOBAL_MEM_FENCE);
#endif


     // run algorithm
     // ----------------------------------------------------------------------------------------------

      C[IDX2(C,0,0)] = A[IDX2(A,0,0)] + B[IDX2(B,0,0)];


#ifdef COPY_HALOS
      // copy outgoing halo regions
      // ----------------------------------------------------------------------------------------------
     hoff_ = 0;
     if (K1_ < HALO1(C,L)) {
        C_H_[hoff_ + K1_ + K2_*HALO1(C,L)] = C[IDX2(C,0,0)];
     }

     hoff_ += HALO1(C,L) * N2_;
     hsft_  = N1_ - HALO1(C,R);
     if (K1_ >= hsft_) {
        C_H_[hoff_ + (K1_ - hsft_) + K2_*HALO1(C,R)] = C[IDX2(C,0,0)];
     }

     hoff_ += HALO1(C,R) * N2_;
     if (K2_ < HALO2(C,L)) {
        C_H_[hoff_ + K1_ + K2_*N1_] = C[IDX2(C,0,0)];
     }

     hoff_ += HALO2(C,L) * N1_;
     hsft_  = N2_ - HALO2(C,R);
     if (K2_ >= hsft_) {
        C_H_[hoff_ + K1_ + (K2_ - hsft_)*N1_] = C[IDX2(C,0,0)];
     }
#endif

   }

}
