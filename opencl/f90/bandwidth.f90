program memory_bandwidth
   use OpenCL
   use Timer_mod
   implicit none

   integer :: status

   ! layer size
   integer(c_size_t), parameter :: NX  = 2048
   integer(c_size_t), parameter :: NY  = 2048
  
   ! work group size
   integer(c_size_t), parameter :: NXL = 16
   integer(c_size_t), parameter :: NYL = 16
   integer(c_size_t) :: nxLocal, nyLocal
   integer(c_int) :: nxg, nyg

   integer(c_size_t), parameter :: SIZE_FLOAT = 4

   real(c_float), target, dimension(NX,NY) :: src, dst

   type(CPUTimer) :: timer
   real(c_double) :: h_time

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_src, d_dst

   integer(cl_bitfield) :: flags
   integer(c_size_t) :: global_mem_size = NX*NY * SIZE_FLOAT
   integer(c_size_t) :: local_mem_size  = NXL*NYL * SIZE_FLOAT

   integer :: device_id, i, j, nLoops, nWarmup
   integer :: d_time = 0
   real :: bandwidth

   device_id = 0

   nWarmup = 20
   nLoops  = 100

   nxg = NX
   nyg = NY
   if (device_id == 0) then
      nxLocal = NXL; nyLocal = NYL
   else
      nxLocal = 1; nyLocal = 1
   end if

   status = init(device, device_id)

   src = 1.1
   dst = 0.0

   ! create memory buffers
   !
   d_src = createBuffer(device, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, &
                        global_mem_size, c_loc(src))
   d_dst = createBuffer(device, CL_MEM_WRITE_ONLY, global_mem_size, C_NULL_PTR)

   ! create the kernel
   !
   kernel = createKernel(device, &
                         "mem_bandwidth.cl"  // C_NULL_CHAR, &
                         "mem_bandwidth"     // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgInt (kernel, 0, nxg) + status
   status = setKernelArgInt (kernel, 1, nyg) + status
   status = setKernelArgMem (kernel, 2, clMemObject(d_src)) + status
   status = setKernelArgMem (kernel, 3, clMemObject(d_dst)) + status

   ! warmup the runtime
   !
   do i = 1, nWarmup
      status = run(kernel, NX, NY, nxLocal, nyLocal)
   end do
   status = clFinish(kernel%commands)

   ! run the kernel on the device
   !
   print *
   print *, "Measuring device thoughput using loads/stores"
   call init(timer)
   call start(timer)
   do i = 1, nLoops
      status = run(kernel, NX, NY, nxLocal, nyLocal) + status
   end do
   status = clFinish(kernel%commands)
   call stop(timer)

   h_time = elapsed_time(kernel%timer)
   print *, "startup time    ==   ", real(h_time)/nLoops, "ms"
   h_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(h_time)/nLoops, "ms"

   ! get the results
   !
   status = readBuffer(d_dst, c_loc(dst), global_mem_size) + status
   if (status /= CL_SUCCESS) print *, "status=", status;  status = 0

   do j = 1, ny
      do i = 1, nx
         if (dst(i,j) /= src(i,j)) then
            print *, "Results incorrect at ", i, j
            stop 1
         end if
      end do
   end do

   if (status == CL_SUCCESS) then
      print *, "Copy correctness verified..."
   end if

   ! 1.0e-9 -> GB, 1000 -> ms, 2 -> to/fro
   bandwidth = (1.0e-9 * 1000) * nLoops * global_mem_size / h_time
   print *, "throughput ==", bandwidth, "GB/s"

   print *
   print *, "Measuring device bandwidth clEnqueueCopyBuffer"
   call init(timer)
   call start(timer)
   do i = 0, nLoops
      status = copyBuffer(d_src, d_dst, global_mem_size) + status
   end do

   status = clFinish(d_src%commands) + status
   if (status /= CL_SUCCESS) print *, "status=", status

   call stop(timer)
   h_time = elapsed_time(timer)
   bandwidth = (1.0e-9 * 1000) * nLoops * global_mem_size / h_time
   print *, "throughput ==", bandwidth, "GB/s"

end program
