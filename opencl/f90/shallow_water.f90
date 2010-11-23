program shallow_water
   use OpenCL
   use Timer_mod
   implicit none

   integer :: status

   integer,           parameter :: NPAD = 1
   integer(c_size_t), parameter :: NX  = 2*1280    ! factor of 2 because of float4
   integer(c_size_t), parameter :: NY  = 2*1280
   integer(c_size_t), parameter :: NXL = 16
   integer(c_size_t), parameter :: NYL = 16

   integer(c_size_t), parameter :: SIZE_ELEMENT = 4

   real(c_float), target, dimension(NX+2*NPAD,NY+2*NPAD) :: H, U, V
   real(c_float) :: dx, dt

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_H, d_U, d_V

   integer(c_size_t) :: nxLocal=NXL, nyLocal=NXL
   integer(c_size_t) :: global_mem_size = NX*NY*SIZE_ELEMENT
   integer(c_size_t) :: global_ex_mem_size = (NX +2*NPAD)*(NY +2*NPAD) * SIZE_ELEMENT
   integer(c_size_t) :: tile_mem_size      = (NXL+2*NPAD)*(NYL+2*NPAD) * SIZE_ELEMENT

   type(CPUTimer) :: timer
   real(c_double) :: h_time

   integer :: nxGlobal=NX, nyGlobal=NY
   integer :: device_id, d_time, i, nLoops=100, nWarm=20
   logical :: check_results
   real :: bandwidth, throughput, flops

   check_results = .false.

   if (NXL < 2*NPAD .or. NYL < 2*NPAD) then
      print *, "thread work group size is too small, die!!!"
      stop 1
   end if

   device_id = 1
   status = init(device, device_id)
   call limitLocalSize(device, nxLocal, nyLocal)

   ! initialize memory
   !

   dx = 1.0;  dt = 0.01;

   H = 1.0;  U = 0.0;  V = 0.0;

   ! create memory buffers
   !
   d_H = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, global_ex_mem_size, c_loc(H))
   d_U = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, global_ex_mem_size, c_loc(U))
   d_V = createBuffer(device, CL_MEM_WRITE_ONLY + CL_MEM_COPY_HOST_PTR, global_ex_mem_size, c_loc(V))

   ! create the kernel
   !
   kernel = createKernel(device, &
                         "shallow_water.cl" // C_NULL_CHAR, &
                         "wave_advance"     // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgMem (kernel, 0, clMemObject(d_H))
   status = setKernelArgMem (kernel, 1, clMemObject(d_U))
   status = setKernelArgMem (kernel, 2, clMemObject(d_V))
   status = setKernelArgReal(kernel, 3, dx)
   status = setKernelArgReal(kernel, 4, dt)
   status = setKernelArgLoc (kernel, 5, 9*tile_mem_size)

print*, "warmup"
   ! warmup the kernel
   !
   do i = 1, nWarm
      status = run(kernel, NX, NY, nxLocal, nyLocal)
   end do
   status = clFinish(kernel%commands)

   ! run the kernel on the device
   !

   print *
   print *, "Measuring flops and effective bandwidth for GPU computation:"
   print *, "global_mem_size ==", &
             global_mem_size, "NX ==", NX, "NY ==", NY, "NPAD ==", NPAD
   call init(timer)
   call start(timer)
   do i = 1, nLoops
      status = run(kernel, NX, NY, nxLocal, nyLocal)
   end do
   status = clFinish(kernel%commands)
   call stop(timer)

   h_time = elapsed_time(kernel%timer)
   print *, " submit time    ==   ", real(h_time)/nLoops
   h_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(h_time)/nLoops, " msec (avg)"

   ! 1.0e-9 -> GB, 1000 -> ms, 2 -> to/fro
   throughput = (1.0e-9 * 1000) * nLoops * (3*global_mem_size / h_time)
   print *, "   throughput   ==    ", throughput, "GB/s"

   ! 1.0e-9 -> GFlop, 1000 -> ms, 5 -> 4 sums / 1 div
   flops = (1.0e-9 * 1000) * nLoops * (100*NX*NY/h_time)
   print *, "   flops        ==    ", flops, "GFlops"

end program shallow_water
