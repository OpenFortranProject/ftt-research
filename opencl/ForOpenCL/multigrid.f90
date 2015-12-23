program multigrid
   use ForOpenCL
   use Timer_mod
   implicit none

   integer :: status
   integer(c_size_t), parameter :: N = 2048, subN = 1
   real(c_float), target, dimension(N+1) :: V1h, Tmp
   real(c_float), target, dimension(N/2+1) :: V2h
   real(c_float), target, dimension(N/4+1) :: V4h
   real(c_float), target, dimension(N/8+1) :: V8h

   type(CLDevice) :: device
   type(CLKernel) :: prolongate_kernel, relax_kernel, restrict_kernel
   type(CLBuffer) :: d_V1h, d_V2h, d_V4h, d_V8h, d_Tmp

   integer(c_size_t), parameter :: SIZE_FLOAT = 4
   integer(c_size_t) :: mem_size_V1h = N+1 * SIZE_FLOAT
   integer(c_size_t) :: mem_size_V2h = N/2+1 * SIZE_FLOAT
   integer(c_size_t) :: mem_size_V4h = N/4+1 * SIZE_FLOAT
   integer(c_size_t) :: mem_size_V8h = N/8+1 * SIZE_FLOAT
   integer(c_size_t) :: mem_size_of_w = 1 * SIZE_FLOAT

   integer :: device_id, i, j, nLoops = N / subN
   real(c_float), target :: w = (2.0/3.0)


   ! TODO: Add AddFourierMode??

   device_id = 1
   status = init_device(device, device_id)
   status = query(device)

   V1h = 1
   Tmp = 0

   ! create memory buffers
   d_V1h = createBuffer(device, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size_V1h, c_loc(V1h))
   d_Tmp = createBuffer(device, CL_MEM_READ_WRITE + CL_MEM_COPY_HOST_PTR, mem_size_V1h, c_loc(Tmp))
   ! create the kernels
   relax_kernel = createKernel(device, "relax", "multigrid.cl")
   ! add arguments
   status = setKernelArgReal(relax_kernel, 0, w) + status
   status = setKernelArgMem(relax_kernel, 1, clMemObject(d_V1h)) + status
   status = setKernelArgMem(relax_kernel, 2, clMemObject(d_Tmp)) + status
   ! run the relax_kernel on the device
   do i = 1, nLoops
      status = run_1D(relax_kernel, N, subN) + status
   end do
   status = clFinish(relax_kernel%commands)
   ! get the results
   status = readBuffer(d_Tmp, c_loc(V1h), mem_size_V1h) + status
   print *, "Result after first relax ", V1h(17), V1h(18), V1h(19), V1h(20)

   ! Restrict to V2h
   d_V2h = createBuffer(device, CL_MEM_WRITE_ONLY + CL_MEM_COPY_HOST_PTR, mem_size_V2h, c_loc(V2h))
   restrict_kernel = createKernel(device, "restrict_kernel", "multigrid.cl")
   status = setKernelArgInt(restrict_kernel, 0, N) + status
   status = setKernelArgMem(restrict_kernel, 1, clMemObject(d_V1h)) + status
   status = setKernelArgMem(restrict_kernel, 2, clMemObject(d_V2h)) + status
   do i = 1, nLoops
      status = run_1D(restrict_kernel, N/2-1, subN) + status
   end do
   status = clFinish(restrict_kernel%commands)
   status = readBuffer(d_V2h, c_loc(V2h), mem_size_V2h) + status

   ! 2nd Relax
   status = setKernelArgReal(relax_kernel, 0, w) + status
   status = setKernelArgMem(relax_kernel, 1, clMemObject(d_V2h)) + status
   status = setKernelArgMem(relax_kernel, 2, clMemObject(d_Tmp)) + status
   do i = 1, nLoops
      status = run_1D(relax_kernel, N/2-1, subN) + status
   end do
   status = clFinish(relax_kernel%commands)
   ! get the results
   status = readBuffer(d_Tmp, c_loc(V2h), mem_size_V2h) + status

   ! Restrict to V4h
   d_V4h = createBuffer(device, CL_MEM_WRITE_ONLY + CL_MEM_COPY_HOST_PTR, mem_size_V4h, c_loc(V4h))
   status = setKernelArgInt(restrict_kernel, 0, N) + status
   status = setKernelArgMem(restrict_kernel, 1, clMemObject(d_V2h)) + status
   status = setKernelArgMem(restrict_kernel, 2, clMemObject(d_V4h)) + status
   do i = 1, nLoops
      status = run_1D(restrict_kernel, N/4-1, subN) + status
   end do
   status = clFinish(restrict_kernel%commands)
   status = readBuffer(d_V4h, c_loc(V4h), mem_size_V4h) + status

   ! 3rd Relax
   status = setKernelArgReal(relax_kernel, 0, w) + status
   status = setKernelArgMem(relax_kernel, 1, clMemObject(d_V4h)) + status
   status = setKernelArgMem(relax_kernel, 2, clMemObject(d_Tmp)) + status
   do i = 1, nLoops
      status = run_1D(relax_kernel, N/4-1, subN) + status
   end do
   status = clFinish(relax_kernel%commands)
   ! get the results
   status = readBuffer(d_Tmp, c_loc(V4h), mem_size_V4h) + status

   ! Restrict to V8h
   d_V8h = createBuffer(device, CL_MEM_WRITE_ONLY + CL_MEM_COPY_HOST_PTR, mem_size_V8h, c_loc(V8h))
   status = setKernelArgInt(restrict_kernel, 0, N) + status
   status = setKernelArgMem(restrict_kernel, 1, clMemObject(d_V4h)) + status
   status = setKernelArgMem(restrict_kernel, 2, clMemObject(d_V8h)) + status
   do i = 1, nLoops
      status = run_1D(restrict_kernel, N/8-1, subN) + status
   end do
   status = clFinish(restrict_kernel%commands)
   status = readBuffer(d_V8h, c_loc(V8h), mem_size_V8h) + status

   ! 4th Relax
   status = setKernelArgReal(relax_kernel, 0, w) + status
   status = setKernelArgMem(relax_kernel, 1, clMemObject(d_V8h)) + status
   status = setKernelArgMem(relax_kernel, 2, clMemObject(d_Tmp)) + status
   do i = 1, nLoops
      status = run_1D(relax_kernel, N/8-1, subN) + status
   end do
   status = clFinish(relax_kernel%commands)
   ! get the results
   status = readBuffer(d_Tmp, c_loc(V8h), mem_size_V8h) + status

   ! Prolongate to V4h
   prolongate_kernel = createKernel(device, "prolongate_kernel", "multigrid.cl")
   status = setKernelArgInt(prolongate_kernel, 0, N) + status
   status = setKernelArgMem(prolongate_kernel, 1, clMemObject(d_V4h)) + status
   status = setKernelArgMem(prolongate_kernel, 2, clMemObject(d_V8h)) + status
   status = run_1D(prolongate_kernel, N/8-1, subN) + status
   status = clFinish(prolongate_kernel%commands)
   status = readBuffer(d_V4h, c_loc(V4h), mem_size_V4h) + status

   ! Prolongate to V2h
   status = setKernelArgInt(prolongate_kernel, 0, N) + status
   status = setKernelArgMem(prolongate_kernel, 1, clMemObject(d_V2h)) + status
   status = setKernelArgMem(prolongate_kernel, 2, clMemObject(d_V4h)) + status
   status = run_1D(prolongate_kernel, N/4-1, subN) + status
   status = clFinish(prolongate_kernel%commands)
   status = readBuffer(d_V2h, c_loc(V2h), mem_size_V2h) + status

   ! Prolongate to V1h
   status = setKernelArgInt(prolongate_kernel, 0, N) + status
   status = setKernelArgMem(prolongate_kernel, 1, clMemObject(d_V1h)) + status
   status = setKernelArgMem(prolongate_kernel, 2, clMemObject(d_V2h)) + status
   status = run_1D(prolongate_kernel, N/2-1, subN) + status
   status = clFinish(prolongate_kernel%commands)
   status = readBuffer(d_V1h, c_loc(V1h), mem_size_V1h) + status




end program
