#include "halos_1d.h"

#define COPY_HALOS

/**
 * simple kernel that computes the sum of array elements
 */
__kernel void elemental_add_1d (__global float * A,  __global float * B,    __global float * C,
                                __global float *A_H_,__global float * B_H_, __global float * C_H_ )
{
   if (K1_ < N1_) {

#ifdef COPY_HALOS
     // copy incoming halo regions (to halo region of array)
     // ----------------------------------------------------------------------------------------------
     if (K1_ < HALO1(C,L)) {
        C[K1_] = C_H_[K1_];
     }
     if (K1_ >= N1_ - HALO1(C,R)) {
        C[IDX1(C,0) + HALO1(C,R)] = C_H_[IDX1(C,0) - (N1_ - HALO1(C,R))];
     }
#endif


     // run algorithm
     // ----------------------------------------------------------------------------------------------

     C[IDX1(C,0)] = A[IDX1(A,0)] + B[IDX1(B,0)];
           

     // copy outgoing halo regions
     // ----------------------------------------------------------------------------------------------

#ifdef COPY_HALOS
     if (K1_ < HALO1(C,L)) {
        C_H_[K1_] = C[IDX1(C,0)];
     }
     if (K1_ >= N1_ - HALO1(C,R)) {
        C_H_[IDX1(C,0) - (N1_ - HALO1(C,R))] = C[IDX1(C,0)];
     }
#endif

   }
}
