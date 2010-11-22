__kernel void mem_bandwidth (int nx, int ny,
    __global float * src,
    __global float * dst )
{
   const unsigned int kx = get_global_id(0);
   const unsigned int ky = get_global_id(1);
   const unsigned int k  = kx + ky*get_global_size(0);

   if (kx < nx && ky < ny) {
      dst[k] = src[k];
   }
}

__kernel void mem4_bandwidth (int nx, int ny,
    __global float4 * src,
    __global float4 * dst )
{
   const unsigned int kx = get_global_id(0);
   const unsigned int ky = get_global_id(1);
   const unsigned int k  = kx + ky*get_global_size(0);

   if (kx < nx && ky < ny/4) {
      dst[k] = src[k];
   }
}