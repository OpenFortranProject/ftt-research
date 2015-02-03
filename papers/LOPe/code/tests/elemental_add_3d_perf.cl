#include "halos_3d_perf.h"

#define COPY_HALOS

/**
 * simple kernel that computes the sum of array elements
 */
__kernel void elemental_add_3d_perf (
    __global float * A,
    __global float * B,
    __global float * C, __global float * C_H_ )
{
   size_t hoff_, hsft_;

   if (K1_ < N1_ && K2_ < N2_ && K3_ < N3_) {

#ifdef COPY_HALOS
      // copy incoming halo regions (to halo region of array)
      // ----------------------------------------------------------------------------------------------
     hoff_ = 0;
     if (K1_ < HALO1(C,L)) {
        C[IDX3(C,-HALO1(C,L),0,0)] = C_H_[hoff_ + K1_ + K2_*HALO1(C,L) + K3_*HALO1(C,L)*N2_];
     }

     hoff_ += HALO1(C,L) * N2_*N3_;
     hsft_  = N1_ - HALO1(C,R);
     if (K1_ >= hsft_) {
        C[IDX3(C,HALO1(C,R),0,0)] = C_H_[hoff_ + (K1_ - hsft_) + K2_*HALO1(C,R) + K3_*HALO1(C,R)*N2_];
     }

     hoff_ += HALO1(C,R) * N2_*N3_;
     if (K2_ < HALO2(C,L)) {
        C[IDX3(C,0,-HALO2(C,L),0)] = C_H_[hoff_ + K1_ + K2_*N1_ + K3_*N1_*HALO2(C,L)];
     }

     hoff_ += HALO2(C,L) * N1_*N3_;
     hsft_  = N2_ - HALO2(C,R);
     if (K2_ >= hsft_) {
        C[IDX3(C,0,HALO2(C,R),0)] = C_H_[hoff_ + K1_ + (K2_ - hsft_)*N1_ + K3_*N1_*HALO2(C,R)];
     }

     hoff_ += HALO2(C,R) * N1_*N3_;
     if (K3_ < HALO3(C,L)) {
        C[IDX3(C,0,0,-HALO3(C,L))] = C_H_[hoff_ + K1_ + K2_*N1_ + K3_*N1_*N2_];
     }

     hoff_ += HALO3(C,L) * N1_*N2_;
     hsft_  = N3_ - HALO3(C,R);
     if (K3_ >= hsft_) {
        C[IDX3(C,0,0,HALO3(C,R))] = C_H_[hoff_ + K1_ + K2_*N1_ + (K3_ - hsft_)*N1_*N2_];
     }

     barrier (CLK_GLOBAL_MEM_FENCE);
#endif


     // run algorithm
     // ----------------------------------------------------------------------------------------------

      C[IDX3(C,0,0,0)] = A[IDX3(A,0,0,0)] + B[IDX3(B,0,1,0)];


#ifdef COPY_HALOS
      // copy outgoing halo regions
      // ----------------------------------------------------------------------------------------------
     hoff_ = 0;
     if (K1_ < HALO1(C,L)) {
        C_H_[hoff_ + K1_ + K2_*HALO1(C,L) + K3_*HALO1(C,L)*N2_] = C[IDX3(C,0,0,0)];
     }

     hoff_ += HALO1(C,L) * N2_*N3_;
     hsft_  = N1_ - HALO1(C,R);
     if (K1_ >= hsft_) {
        C_H_[hoff_ + (K1_ - hsft_) + K2_*HALO1(C,R) + K3_*HALO1(C,R)*N2_] = C[IDX3(C,0,0,0)];
     }

     hoff_ += HALO1(C,R) * N2_*N3_;
     if (K2_ < HALO2(C,L)) {
        C_H_[hoff_ + K1_ + K2_*N1_ + K3_*N1_*HALO2(C,L)] = C[IDX3(C,0,0,0)];
     }

     hoff_ += HALO2(C,L) * N1_*N3_;
     hsft_  = N2_ - HALO2(C,R);
     if (K2_ >= hsft_) {
        C_H_[hoff_ + K1_ + (K2_ - hsft_)*N1_ + K3_*N1_*HALO2(C,R)] = C[IDX3(C,0,0,0)];
     }

     hoff_ += HALO2(C,R) * N1_*N3_;
     if (K3_ < HALO3(C,L)) {
        C_H_[hoff_ + K1_ + K2_*N1_ + K3_*N1_*N2_] = C[IDX3(C,0,0,0)];
     }

     hoff_ += HALO3(C,L) * N1_*N2_;
     hsft_  = N3_ - HALO3(C,R);
     if (K3_ >= hsft_) {
        C_H_[hoff_ + K1_ + K2_*N1_ + (K3_ - hsft_)*N1_*N2_] = C[IDX3(C,0,0,0)];
     }
#endif

   }

}
