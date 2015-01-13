#include "halos.h"

/**
 * simple kernel that computes the sum of array elements
 */
__kernel void elemental_add_1d (__global float * A,  __global float * B,    __global float * C,
                                __global float *A_H_,__global float * B_H_, __global float * C_H_ )
{
   if (K1_ < N1_) {


     // copy incoming halo regions
     // ----------------------------------------------------------------------------------------------
     if (K1_ < HALO1_L(A)) {
        A[K1_] = A_H_[K1_];
     }
     if (K1_ >= N1_ - HALO1_R(A)) {
        A[IDX1(0,A) + HALO1_R(A)] = A_H_[IDX1(0,A) - (N1_ - HALO1_R(A))];
     }
     if (K1_ < HALO1_L(B)) {
        B[K1_] = B_H_[K1_];
     }
     if (K1_ >= N1_ - HALO1_R(B)) {
        B[IDX1(0,B) + HALO1_R(B)] = B_H_[IDX1(0,B) - (N1_ - HALO1_R(B))];
     }


     // run algorithm
     // ----------------------------------------------------------------------------------------------

     C[IDX1(0,C)] = A[IDX1(0,A)] + B[IDX1(0,B)];
           

     // copy outgoing halo regions
     // ----------------------------------------------------------------------------------------------

     if (K1_ < C_H1L_) {
        C_H_[K1_] = C[IDX1(0,C)];
     }
     if (K1_ >= N1_ - HALO1_R(C)) {
        C_H_[IDX1(0,C) - (N1_ - HALO1_R(C))] = C[IDX1(0,C)];
     }

   }
}
