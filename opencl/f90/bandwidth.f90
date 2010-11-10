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
   real(c_float), pointer, dimension(:,:)  :: p_src, p_dst

   type(CPUTimer) :: timer
   real(c_double) :: cpu_time

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_src, d_dst
   type(c_ptr)    :: h_src, h_dst

   integer(cl_bitfield) :: flags
   integer(c_size_t) :: global_mem_size = NX*NY * SIZE_FLOAT
   integer(c_size_t) :: local_mem_size  = NXL*NYL * SIZE_FLOAT

   integer :: device_id, i, nLoops
   integer :: ocl_time = 0
   real :: bandwidth

   device_id = 0
   nLoops = 20

   nxg = NX
   nyg = NY
   if (device_id == 0) then
      nxLocal = NXL; nyLocal = NYL
   else
      nxLocal = 1; nyLocal = 1
   end if

   status = init(device, device_id)

   ! create memory buffers
   !
   d_src = createBuffer(device, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, &
                        global_mem_size, c_loc(src))
   d_dst = createBuffer(device, CL_MEM_WRITE_ONLY, global_mem_size, C_NULL_PTR)

!   d_src = createBufferMapped(device, global_mem_size, c_loc(src))
!   d_dst = createBufferMapped(device, global_mem_size, c_loc(dst))

   ! map memory so that it can be initialized on host
   !
!   h_src = map(d_src, CL_MAP_WRITE)
!   call c_f_pointer(h_src, p_src, shape(src))

   src = 1.1

   ! finished initializing memory, unmap for use on device
   !
!   status = unmap(d_src)

   ! create the kernel
   !
   kernel = createKernel(device, &
                         "mem_bandwidth.cl"  // C_NULL_CHAR, &
                         "mem_bandwidth" // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgInt (kernel, 0, nxg) + status
   status = setKernelArgInt (kernel, 1, nyg) + status
   status = setKernelArgMem (kernel, 2, clMemObject(d_src)) + status
   status = setKernelArgMem (kernel, 3, clMemObject(d_dst)) + status
!   status = setKernelArgLoc (kernel, 4, local_size) + status

   ! run the kernel on the device
   !
   print *
   print *, "Measuring device bandwidth using loads/stores"
   call init_timer(timer)
   call start(timer)
   do i = 0, nLoops
      status = run(kernel, NX, NY, nxLocal, nyLocal) + status
      if (i > 0) then
         ocl_time = ocl_time + kernel%elapsed/1000
      end if
   end do
   call stop(timer)
   call print_elapsed_time(timer)
   print *, "opencl timer==", ocl_time, "ms"

   ! get the results
   !
!   h_dst = map(d_dst, CL_MAP_READ)
!   call c_f_pointer(h_dst, p_dst, shape(dst))

   if (status /= CL_SUCCESS) print *, "status=", status

   ! 1.0e-9 -> GB, 1000 -> ms, 2 -> to/fro
   bandwidth = (1.0e-9 * 1000) * nLoops * (2*global_mem_size / ocl_time)
   print *, "bandwidth ==", bandwidth, "GB/s"

   print *
   print *, "Measuring device bandwidth clEnqueueCopyBuffer"
   call init_timer(timer)
   call start(timer)
   do i = 0, nLoops
      status = copyBuffer(d_src, d_dst, global_mem_size) + status
   end do

   status = clFinish(d_src%commands) + status
   if (status /= CL_SUCCESS) print *, "status=", status

   call stop(timer)
   cpu_time = elapsed_time(timer)
   bandwidth = (1.0e-9 * 1000) * nLoops * (2*global_mem_size / cpu_time)
   print *, "bandwidth ==", bandwidth, "GB/s"

end program
