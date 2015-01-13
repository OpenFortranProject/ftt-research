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
     if (K1_ < HALO1(A,L)) {
        A[K1_] = A_H_[K1_];
     }
     if (K1_ >= N1_ - HALO1(A,R)) {
        A[IDX1(0,A) + HALO1(A,R)] = A_H_[IDX1(0,A) - (N1_ - HALO1(A,R))];
     }
     if (K1_ < HALO1(B,L)) {
        B[K1_] = B_H_[K1_];
     }
     if (K1_ >= N1_ - HALO1(B,R)) {
        B[IDX1(0,B) + HALO1(B,R)] = B_H_[IDX1(0,B) - (N1_ - HALO1(B,R))];
     }


     // run algorithm
     // ----------------------------------------------------------------------------------------------

     C[IDX1(0,C)] = A[IDX1(0,A)] + B[IDX1(0,B)];
           

     // copy outgoing halo regions
     // ----------------------------------------------------------------------------------------------

     if (K1_ < HALO1(C,L)) {
        C_H_[K1_] = C[IDX1(0,C)];
     }
     if (K1_ >= N1_ - HALO1(C,R)) {
        C_H_[IDX1(0,C) - (N1_ - HALO1(C,R))] = C[IDX1(0,C)];
     }

   }
}
