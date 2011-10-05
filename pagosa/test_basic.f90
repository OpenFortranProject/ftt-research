module Params
   use, intrinsic :: ISO_C_BINDING

   integer(c_size_t), parameter :: mx  = 48
   integer(c_size_t), parameter :: my  = 48
   integer(c_size_t), parameter :: mz  = 48
   integer(c_size_t), parameter :: np  = 2     ! padding for halo cells
   integer(c_size_t), parameter :: NXL = 16
   integer(c_size_t), parameter :: NYL = 8
   integer(c_size_t), parameter :: ELEM_SIZE = 4
end module Params

program test_filter
   use Params
   use OpenCL
   use Timer_mod
   implicit none

   integer :: status

   real(c_float), target, dimension(0:mx, 0:my, 0:mz)  :: Array, Eps, dFlux, 
   real(c_float), target, dimension(NX,NY)               :: Q
   real(c_float), target, dimension(1+2*NPAD,1+2*NPAD)   :: F

   integer(c_size_t) :: tile_mem_size = (NXL+2*NPAD)*(NYL+2*NPAD) * ELEM_SIZE

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: P_d, Q_d, F_d

   integer(c_size_t) :: nxLocal=NXL, nyLocal=NXL

   type(CPUTimer) :: timer
   real(c_double) :: h_time

   integer :: nxGlobal=NX, nyGlobal=NY
   integer :: device_id, d_time, i, nLoops=100, nWarm=0
   real    :: bandwidth, throughput, flops




   device_id = 1
   status = init(device, device_id)
   call limitLocalSize(device, nxLocal, nyLocal)
   print *, "device_id  ==", device_id
   print *, "local size ==", nxLocal, nyLocal

   ! initialize memory
   !

   P = 1.0;  Q = 0.0;  F = 0.0;

   ! create memory buffers
   !
   P_d = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, size(P)*ELEM_SIZE, c_loc(P))
   Q_d = createBuffer(device, CL_MEM_WRITE_ONLY + CL_MEM_COPY_HOST_PTR, size(Q)*ELEM_SIZE, c_loc(Q))
   F_d = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, size(F)*ELEM_SIZE, c_loc(F))

   ! create the kernel
   !
   kernel = createKernel(device, &
                         "apply_filter.cl" // C_NULL_CHAR, &
                         "apply_filter"    // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgMem (kernel, 0, clMemObject(P_d))
   status = setKernelArgMem (kernel, 1, clMemObject(Q_d))
   status = setKernelArgMem (kernel, 2, clMemObject(F_d))
   status = setKernelArgLoc (kernel, 3, tile_mem_size)

   ! run the kernel on the device
   !

   call init(timer)
   call start(timer)
   do i = 1, nLoops
      status = run(kernel, NX, NY, nxLocal, nyLocal)
   end do
   status = clFinish(kernel%commands)
   call stop(timer)

   ! get the results
   !
   status = readBuffer(Q_d, c_loc(Q), size(Q)*ELEM_SIZE)

   h_time = elapsed_time(kernel%timer)
   print *, " submit time    ==   ", real(h_time)/nLoops
   h_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(h_time)/nLoops, " msec (avg)"

!   call check_results(P, Q, F)

end program


subroutine check_results(A, B, C, s)
   use Params
   integer :: i, j
   real, dimension(NX,NY) :: A, B, C
   real :: s

   do j = 1, NY
      do i = 1, NX
         !print *, i, j, C(i,j)
         if (C(i,j) /= A(i,j) + s*B(i,j)) then
            print *, "Error at", i, j
            return
         end if
      end do
   end do

   print *
   print *, "The elemental_add kernel completed successfully"
   print *

end subroutine check_results
