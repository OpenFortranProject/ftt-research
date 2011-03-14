/**
 * simple kernel that computes the sum of array elements
 */
__kernel void elemental_add (
    __global float * A,
    __global float * B,
    __global float * C )
{
   const unsigned int kx = get_global_id(0);
   const unsigned int ky = get_global_id(1);
   const unsigned int k  = kx + ky*get_global_size(0);

   if (k < get_global_size(0)*get_global_size(1)) {
      C[k] = A[k] + B[k];
   }
}
