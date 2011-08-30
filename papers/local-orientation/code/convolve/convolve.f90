module convolve_mod
   use ISO_C_BINDING

   integer, parameter :: G_WIDTH = 1280

   ! WARNING, must also change in convolve.cl
   integer(c_int), parameter :: NPAD = 7

   integer(c_int), parameter :: NXP = 1 + 2*NPAD
   integer(c_int), parameter :: NYP = NXP

   integer(c_size_t), parameter :: NX   = G_WIDTH
   integer(c_size_t), parameter :: NY   = G_WIDTH
   integer(c_size_t), parameter :: NXEX = NX + 2*NPAD
   integer(c_size_t), parameter :: NYEX = NY + 2*NPAD
   integer(c_size_t), parameter :: NXL  = 16
   integer(c_size_t), parameter :: NYL  = 16

   integer(c_size_t), parameter :: SIZE_ELEMENT = 4

contains

   subroutine convolve(S, Image, F)
      implicit none
      real :: S(NX,NY), Image(NXEX,NYEX), F(NXP,NYP)
      real :: val
      integer :: i, j, ip, jp

      do j = 1, NY
         do i = 1, NX
            val = 0.0
            do jp = 1, NYP
               do ip = 1, NXP
                  val = val + F(ip, jp)*Image(i+NPAD,j+NPAD)
               end do
            end do
            S(i,j) = val
         end do
      end do
   end subroutine convolve

   subroutine init_filter(F)
      implicit none
      real :: F(NXP,NYP)
      integer :: i

      F = 0.0
      F(:,NYP/2) = 1
      F(NXP/2,:) = 1
      F = F/sum(F)

    end subroutine init_filter

end module convolve_mod


program test_convolve
   use convolve_mod
   use OpenCL
   use Timer_mod
   implicit none

   integer :: status

   real(c_float), target, dimension(NX+2*NPAD,NY+2*NPAD) :: I  ! image
   real(c_float), target, dimension(NX,NY)               :: S  ! smoothed image
   real(c_float), target, dimension(NXP,NYP)             :: F  ! filter

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_I, d_S, d_F

   integer(c_size_t) :: nxLocal=NXL, nyLocal=NXL
   integer(c_size_t) :: global_mem_size    = NX*NY*SIZE_ELEMENT
   integer(c_size_t) :: global_ex_mem_size = NXEX*NYEX*SIZE_ELEMENT
   integer(c_size_t) :: tile_mem_size      = (NXL+2*NPAD)*(NYL+2*NPAD) * SIZE_ELEMENT

   type(CPUTimer) :: timer
   real(c_double) :: cpu_time, gpu_time

   integer :: nxGlobal=NX, nyGlobal=NY
   integer :: ii, jj
   integer :: device_id, d_time, nLoops=100, nWarm=20
   logical :: check_results
   real :: bandwidth, throughput, flops

   check_results = .false.

   if (NXL < 2*NPAD .or. NYL < 2*NPAD) then
      print *, "thread work group size is too small, die!!!"
      stop 1
   end if

   device_id = 0  ! 0=GPU, 1=CPU
   status = init(device, device_id)
   call limitLocalSize(device, nxLocal, nyLocal)
   print *, "device_id   ==", device_id
   print *, "local size  ==", nxLocal, nyLocal
   print *, "global size ==", NX, NY
   print *, "tile_mem_size   ==", tile_mem_size
   print *, "global_mem_size ==", global_mem_size

   ! initialize memory
   !

   I = 1.0;  S = 0.0;
   call init_filter(F)

   ! create memory buffers
   !
   d_I = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, global_ex_mem_size,   c_loc(I))
   d_S = createBuffer(device, CL_MEM_WRITE_ONLY + CL_MEM_COPY_HOST_PTR, global_mem_size,      c_loc(S))
   d_F = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, NXP*NYP*SIZE_ELEMENT, c_loc(F))

   ! create the kernel
   !
   kernel = createKernel(device, &
                         "convolve.cl" // C_NULL_CHAR, &
                         "convolve"    // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgInt(kernel, 0, NXP)
   status = setKernelArgInt(kernel, 1, NYP)
   status = setKernelArgMem(kernel, 2, clMemObject(d_I))
   status = setKernelArgMem(kernel, 3, clMemObject(d_S))
   status = setKernelArgMem(kernel, 4, clMemObject(d_F))
   status = setKernelArgLoc(kernel, 5, 1*tile_mem_size)

   ! warmup the kernel
   !
   print *, "warmup"
   do ii = 1, nWarm
      status = run(kernel, NX, NY, nxLocal, nyLocal)
   end do
   status = clFinish(kernel%commands)

   ! run the kernel on the device
   !

   print *
   print *, "Measuring flops and effective bandwidth for GPU computation:"
   call init(timer)
   call start(timer)
   do ii = 1, nLoops
      status = run(kernel, NX, NY, nxLocal, nyLocal)
   end do
   status = clFinish(kernel%commands)
   call stop(timer)

   gpu_time = elapsed_time(kernel%timer)
   print *, " submit time    ==   ", real(gpu_time)/nLoops
   gpu_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(gpu_time)/nLoops, " msec (avg)"

   ! 1.0e-9 -> GB, 1000 -> ms, 2 -> to/fro
   throughput = (1.0e-9 * 1000) * nLoops * (2*global_mem_size / gpu_time)
   print *, "   throughput   ==    ", throughput, "GB/s"

   ! 1.0e-9 -> GFlop, 1000 -> ms, 5 -> 4 sums / 1 div
   flops = (1.0e-9 * 1000) * nLoops * (2*NXP*NYP*NX*NY/gpu_time)
   print *, "   flops        ==    ", flops, "GFlops"

   ! get the results
   !
   status = readBuffer(d_S, c_loc(S), global_mem_size)
   
   print *, S(1, 1:3)
   do ii = 1, NX
      do jj = 1, NY
         if (abs(S(ii,jj) -1.0) > .000001) then
            print *, "ERROR FOUND at", ii, jj, "=", S(ii,jj)
            STOP
         endif
      end do
   end do

   call init(timer)
   call start(timer)
   call convolve(S, I, F)
   call stop(timer)

   cpu_time = elapsed_time(timer)
   print *, S(1, 1:3)
   print *, "   host time    ==   ", real(cpu_time), " msec"
   print *, "   SPEEDUP      ==", cpu_time/(gpu_time/nLoops)

   do ii = 1, NX
      do jj = 1, NY
         if (abs(S(ii,jj) -1.0) > .000001) then
            print *, "ERROR FOUND at", ii, jj, "=", S(ii,jj)
            STOP
         endif
      end do
   end do

end program test_convolve
