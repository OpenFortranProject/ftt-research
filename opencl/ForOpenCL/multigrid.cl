__kernel void relax_1d_kernel (const float w, __global float * A, __global float * B)
{
   const unsigned int i = get_global_id(0) + 1;
   
   B[i] = (1.0 - w)*A[i] + 0.5*w*(A[i-1] + A[i+1]);
}


__kernel void restrict_1d_kernel (const float n, __global float * V1h, __global float * V2h)
{
   const unsigned int i = get_global_id(0) + 1;
   int m = n / 2 - 1, ii = 2*i;
   
   
   V2h[i] = 0.25*(V1h[ii-1] + 2.0*V1h[ii] + V1h[ii+1]);
}

__kernel void prolongate_1d_kernel (const float n, __global float * V1h, __global float * V2h)
{
   const unsigned int i = get_global_id(0) + 1;
   int m = n / 2 - 1, ii = 2*i;

   V1h[ii] = V2h[i];
   V1h[ii+1] = 0.5*(V2h[i] + V2h[i+1]);
}


// DO WE WANT TO PASS N? Thus 3D index will be i*n + j*n + k?


/* 3D kernels*/
__kernel void relax_3d_kernel (const float w, __global float * A, __global float * B)
{
   const unsigned int i = get_global_id(0) + 1;
   const unsigned int j = get_global_id(1) + 1;
   const unsigned int k = get_global_id(2) + 1;
   
   B[i] = (1.0 - w)*A[i] + 0.5*w*(A[i-1] + A[i+1]);
}

__kernel void restrict_3d_kernel (const float n, __global float * V1h, __global float * V2h)
{
   const unsigned int i = get_global_id(0) + 1;
   const unsigned int j = get_global_id(1) + 1;
   const unsigned int k = get_global_id(2) + 1;

   int m = n / 2 - 1, ii = 2*i;
   
   
   V2h[i] = 0.25*(V1h[ii-1] + 2.0*V1h[ii] + V1h[ii+1]);
}

__kernel void prolongate_3d_kernel (const float n, __global float * V1h, __global float * V2h)
{
   const unsigned int i = get_global_id(0) + 1;
   const unsigned int j = get_global_id(1) + 1;
   const unsigned int k = get_global_id(2) + 1;
   int m = n / 2 - 1, ii = 2*i;

   V1h[ii] = V2h[i];
   V1h[ii+1] = 0.5*(V2h[i] + V2h[i+1]);
}



