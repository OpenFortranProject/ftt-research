#pragma OPENCL EXTENSION cl_khr_global_int32_base_atomics : enable

#define KX  get_global_id(0)
#define KY  get_global_id(1)
#define KXL get_local_id(0)
#define KYL get_local_id(1)

#define NX  get_global_size(0)
#define NY  get_global_size(1)

__kernel void atomic_inc (__global int * W)
{
//   atom_inc(&W[0]);
   atom_add(W, 2);
}
