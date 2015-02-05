#include "halos_1d.h"

#define COPY_HALOS

/**
 * simple kernel that computes the sum of array elements
 */
__kernel void elemental_add_1d (__global float * A,  __global float * B,    __global float * C,
                                __global float * C_H_ )
{
   size_t hoff_, hsft_;

   if (K1_ < N1_) {

#ifdef COPY_HALOS
     // copy incoming halo regions (to halo region of array)
     // ----------------------------------------------------------------------------------------------
     hoff_ = 0;
     if (K1_ < HALO1(C,L)) {
        C[IDX1(C,-HALO1(C,L))] = C_H_[hoff_ + K1_];
     }

     hoff_ += HALO1(C,L);
     hsft_  = N1_ - HALO1(C,R);
     if (K1_ >= hsft_) {
        C[IDX1(C,HALO1(C,R))] = C_H_[hoff_ + (K1_ - hsft_)]; 
     }

     barrier (CLK_GLOBAL_MEM_FENCE);
#endif


     // run algorithm
     // ----------------------------------------------------------------------------------------------

     C[IDX1(C,0)] = A[IDX1(A,0)] + B[IDX1(B,0)];


     // copy outgoing halo regions
     // ----------------------------------------------------------------------------------------------

#ifdef COPY_HALOS
     hoff_ = 0;
     if (K1_ < HALO1(C,L)) {
        C_H_[hoff_ + K1_] = C[IDX1(C,0)];
     }

     hoff_ += HALO1(C,L);
     hsft_  = N1_ - HALO1(C,R);
     if (K1_ >= hsft_) {
        C_H_[hoff_ + (K1_ - hsft_)] = C[IDX1(C,0)];
     }
#endif

   }
}
