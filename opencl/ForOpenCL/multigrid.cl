/**
 * simple kernel that computes the sum of array elements
 */
__kernel void relax (const float w, __global float * A, __global float * B)
{
   const unsigned int i = get_global_id(0) + 1;
   
   B[i] = (1.0 - w)*A[i] + 0.5*w*(A[i-1] + A[i+1]);
}

/**
 * simple kernel that computes the sum of array elements
 */

__kernel void restrict_kernel (const float n, __global float * V1h, __global float * V2h)
{
   const unsigned int i = get_global_id(0) + 1;
   int m = n / 2 - 1, ii = 2*i;
   
   
   V2h[i] = (0.25)*V1h[ii-1] + 2.0*V1h[ii] + V1h[ii+1];
}

__kernel void prolongate_kernel (const float n, __global float * V1h, __global float * V2h)
{
   const unsigned int i = get_global_id(0) + 1;
   int m = n / 2 - 1, ii = 2*i;

   V1h[ii] = V2h[i];
   V1h[ii+1] = 0.5*(V2h[i] + V2h[i+1]);
}

