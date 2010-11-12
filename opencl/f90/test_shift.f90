program test_shift
   use OpenCL
   use Timer_mod
   implicit none

   interface
      subroutine shift_f90(A, B, nPad)
         implicit none
         real, dimension(:,:) :: A, B
         integer :: nPad
      end subroutine shift_f90
      subroutine loops_f90(A, B, nPad)
         implicit none
         real, dimension(:,:) :: A, B
         integer :: nPad
      end subroutine loops_f90
      subroutine loops_c(A, B, nx, ny, nPad) bind(C,name="loops_c")
         implicit none
         real, dimension(*) :: A, B
         integer, value :: nx, ny, nPad
      end subroutine loops_c
   end interface

   integer :: status

   integer(c_size_t), parameter :: SCALE = 4
   integer(c_size_t), parameter :: NX  = SCALE*512
   integer(c_size_t), parameter :: NY  = SCALE*512
   integer(c_size_t), parameter :: NXL = 16
   integer(c_size_t), parameter :: NYL = 16
   integer,           parameter :: NPAD = 1

   integer(c_size_t), parameter :: SIZE_ELEMENT = 4

   !integer, target, dimension(NX+2*NPAD,NY+2*NPAD) :: A, B, C
   real, target, dimension(NX+2*NPAD,NY+2*NPAD) :: A, B, C

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_A, d_B, d_C

   integer(c_size_t) :: nxLocal=NXL, nyLocal=NXL
   integer(c_size_t) :: global_mem_size = NX*NY*SIZE_ELEMENT
   integer(c_size_t) :: global_ex_mem_size = (NX +2*NPAD)*(NY +2*NPAD) * SIZE_ELEMENT
   integer(c_size_t) :: local_ex_mem_size  = (NXL+2*NPAD)*(NYL+2*NPAD) * SIZE_ELEMENT

   type(CPUTimer) :: timer
   real(c_double) :: h_time

   integer :: nxGlobal=NX, nyGlobal=NY
   integer :: device_id, d_time, i, nLoops=1
   logical :: check_results
   real :: bandwidth, flops

   check_results = .false.

   if (NXL < 2*NPAD .or. NYL < 2*NPAD) then
      print *, "thread work group size is too small, die!!!"
      stop 1
   end if

   device_id = 0
   status = init(device, device_id)
   call limitLocalSize(device, nxLocal, nyLocal)

   ! initialize memory
   !
   A = 0
   B = 0
   C = 0

   A(2:NX+NPAD, 2:NY+NPAD) = 1
   B(2:NX+NPAD, 2:NY+NPAD) = 2

   ! create memory buffers
   !
   d_A = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, global_ex_mem_size, c_loc(A))
   d_B = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, global_ex_mem_size, c_loc(B))
   d_C = createBuffer(device, CL_MEM_WRITE_ONLY + CL_MEM_COPY_HOST_PTR, global_ex_mem_size, c_loc(C))

   ! create the kernel
   !
   kernel = createKernel(device, &
                         "shift.cl" // C_NULL_CHAR, &
                         "shift" // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgInt(kernel, 0, NPAD)
   status = setKernelArgMem(kernel, 1, clMemObject(d_A))
   status = setKernelArgMem(kernel, 2, clMemObject(d_B))
   status = setKernelArgMem(kernel, 3, clMemObject(d_C))
   status = setKernelArgLoc(kernel, 4, local_ex_mem_size)

   ! run the kernel on the device
   !

   print *
   print *, "Measuring flops and effective bandwidth of computation"
   d_time = 0
   call init_timer(timer)
   call start(timer)
   do i = 1, nLoops
      status = run(kernel, NX, NY, nxLocal, nyLocal)
      !print *, "       device timer==", kernel%elapsed, "microsec"
      d_time = d_time + kernel%elapsed
   end do

   call stop(timer)
   h_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(h_time), " msec"
   print *, "   device timer ==", d_time, "        usec"

   ! 1.0e-9 -> GB, 1000 -> ms, 2 -> to/fro
   bandwidth = (1.0e-9 * 1000) * nLoops * (2*global_mem_size / (d_time/1000))
   print *, "   bandwidth    ==    ", bandwidth, "GB/s"

   ! 1.0e-9 -> GFlop, 1000 -> ms, 5 -> 4 sums / 1 div
   flops = (1.0e-9 * 1000) * nLoops * (5*NX*NY/h_time)
   print *, "   flops        ==    ", flops, "GFlops"

   ! get the results
   !
   status = readBuffer(d_C, c_loc(C), global_ex_mem_size)

   print *
   print *, "Measuring flops and effective bandwidth on CPU for Fortran arrays"
   call init_timer(timer)
   call start(timer)
   do i = 1, nLoops
      call shift_f90(A, C, nPad)
   end do
   call stop(timer)
   h_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(h_time)

   ! 1.0e-9 -> GB, 1000 -> ms, 2 -> to/fro
   bandwidth = (1.0e-9 * 1000) * nLoops * (2*global_mem_size / h_time)
   print *, "   bandwidth    ==    ", bandwidth, "GB/s"

   ! 1.0e-9 -> GFlop, 1000 -> ms, 5 -> 4 sums / 1 div
   flops = (1.0e-9 * 1000) * nLoops * (5*NX*NY/h_time)
   print *, "   flops        ==    ", flops, "GFlops"
   print *

   print *
   print *, "Measuring flops and effective bandwidth on CPU for C loops"
   call init_timer(timer)
   call start(timer)
   do i = 1, nLoops
      call loops_c(A, C, nxGlobal, nyGlobal, nPad)
   end do
   call stop(timer)
   h_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(h_time)

   ! 1.0e-9 -> GB, 1000 -> ms, 2 -> to/fro
   bandwidth = (1.0e-9 * 1000) * nLoops * (2*global_mem_size / h_time)
   print *, "   bandwidth    ==    ", bandwidth, "GB/s"

   ! 1.0e-9 -> GFlop, 1000 -> ms, 5 -> 4 sums / 1 div
   flops = (1.0e-9 * 1000) * nLoops * (5*NX*NY/h_time)
   print *, "   flops        ==    ", flops, "GFlops"
   print *

   print *
   print *, "Measuring flops and effective bandwidth on CPU for Fortran loops"
   call init_timer(timer)
   call start(timer)
   do i = 1, nLoops
      call loops_f90(A, C, nPad)
   end do
   call stop(timer)
   h_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(h_time)

   ! 1.0e-9 -> GB, 1000 -> ms, 2 -> to/fro
   bandwidth = (1.0e-9 * 1000) * nLoops * (2*global_mem_size / h_time)
   print *, "   bandwidth    ==    ", bandwidth, "GB/s"

   ! 1.0e-9 -> GFlop, 1000 -> ms, 5 -> 4 sums / 1 div
   flops = (1.0e-9 * 1000) * nLoops * (5*NX*NY/h_time)
   print *, "   flops        ==    ", flops, "GFlops"
   print *

   if (check_results) then
      print *, "external corners"
      print *, C(1,1), " =", A(1,1), " +", B(1,1)
      print *, C(1,NX+2*NPAD), " =", A(1,NX+2*NPAD), " +", B(1,NX+2*NPAD)
      print *, C(NX+2*NPAD,1), " =", A(NX+2*NPAD,1), " +", B(NX+2*NPAD,1)
      print *, C(NX+2*NPAD,NX+2*NPAD), " =", A(NX+2*NPAD,NX+2*NPAD), " +", B(NX+2*NPAD,NX+2*NPAD)

      print *, "internal corners"
      print *, C(1+NPAD,1+NPAD), " =", A(1+NPAD,1+NPAD), " +", B(1+NPAD,1+NPAD)
      print *, C(1+NPAD,NX+1*NPAD), " =", A(1+NPAD,NX+1*NPAD), " +", B(1+NPAD,NX+1*NPAD)
      print *, C(NX+1*NPAD,1+NPAD), " =", A(NX+1*NPAD,1+NPAD), " +", B(NX+1*NPAD,1+NPAD)
      print *, C(NX+1*NPAD,NX+1*NPAD), " =", A(NX+1*NPAD,NX+1*NPAD), " +", B(NX+1*NPAD,NX+1*NPAD)

      print *, "partial corners"
      print *, C(1+NPAD,2+NPAD), " =", A(1+NPAD,2+NPAD), " +", B(1+NPAD,2+NPAD)
      print *, C(1+NPAD,NX+0*NPAD), " =", A(1+NPAD,NX+0*NPAD), " +", B(1+NPAD,NX+0*NPAD)
      print *, C(NX+1*NPAD,2+NPAD), " =", A(NX+1*NPAD,2+NPAD), " +", B(NX+1*NPAD,2+NPAD)
      print *, C(NX+1*NPAD,NX+0*NPAD), " =", A(NX+1*NPAD,NX+0*NPAD), " +", B(NX+1*NPAD,NX+0*NPAD)

      print *, "internal-1 corners"
      print *, C(2+NPAD,2+NPAD), " =", A(2+NPAD,2+NPAD), " +", B(2+NPAD,2+NPAD)
      print *, C(2+NPAD,NX+0*NPAD), " =", A(2+NPAD,NX+0*NPAD), " +", B(2+NPAD,NX+0*NPAD)
      print *, C(NX+0*NPAD,2+NPAD), " =", A(NX+0*NPAD,2+NPAD), " +", B(NX+0*NPAD,2+NPAD)
      print *, C(NX+0*NPAD,NX+0*NPAD), " =", A(NX+0*NPAD,NX+0*NPAD), " +", B(NX+0*NPAD,NX+0*NPAD)

      print *, "interior points"
      print *, C(5,5), " =", A(5,5), " +", B(5,5)
      print *, C(48,48), " =", A(48,48), " +", B(48,48)
   end if

end program test_shift
