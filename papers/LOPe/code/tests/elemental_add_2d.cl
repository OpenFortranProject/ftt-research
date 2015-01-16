#include "halos.h"

/**
 * simple kernel that computes the sum of array elements
 */
__kernel void elemental_add_2d (
    __global float * A,
    __global float * B,
    __global float * C, __global float * C_H_ )
{
   const unsigned int k  = K1_ + K2_*N1_;

   if (K1_ < N1_ && K2_ < N2_) {
      C[k] = A[k] + B[k];
   }

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


}
