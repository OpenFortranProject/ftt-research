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
     if (K1_ < A_H1L_) {
        A[K1_] = A_H_[K1_];
     }
     if (K1_ >= N1_ - A_H1R_) {
        A[K1_ + A_H1R_ + A_H1L_] = A_H_[K1_ - (N1_ - A_H1R_) + A_H1L_];
     }
     if (K1_ < B_H1L_) {
        B[K1_] = B_H_[K1_];
     }
     if (K1_ >= N1_ - B_H1R_) {
        B[K1_ + B_H1R_ + B_H1L_] = B_H_[K1_ - (N1_ - B_H1R_) + B_H1L_];
     }

     // run algorithm
     // ----------------------------------------------------------------------------------------------

     C[(0 + K1_ + C_H1L_)*C_S1_] = A[(0 + K1_ + A_H1L_)*A_S1_]
                                 + B[(0 + K1_ + B_H1L_)*B_S1_];
                                 
           
     // copy outgoing halo regions
     // ----------------------------------------------------------------------------------------------

     if (K1_ < C_H1L_) {
        C_H_[K1_] = C[K1_ + C_H1L_];
     }
     if (K1_ >= N1_ - C_H1R_) {
        C_H_[K1_ - (N1_ - C_H1R_) + C_H1L_] = C[K1_ + C_H1L_];
     }

   }
}
