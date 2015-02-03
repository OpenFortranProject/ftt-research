program elemental_add_1d
!...TODO-GENERATE
   use ForOpenCL
   use Parallel_Halo , only : Context, Parallel_Start, Parallel_End, Parallel_Topology
   use Parallel      , only : Halo_Exchange1D
!...TODO-END-GENERATE
   use Timer_mod
   implicit none

!   integer(c_size_t), parameter :: NX  = 16*1024*1024
   integer(c_size_t), parameter :: NX  = 16

!...TODO-GENERATE
   integer :: cl_status_

   integer(c_size_t), parameter :: NH  = 2

   integer(c_size_t), parameter :: SIZE_FLOAT = 4
   integer(c_size_t), parameter :: N_HALO_ELEM = 2*NH
   integer(c_size_t), parameter ::   HALO_SIZE = N_HALO_ELEM*SIZE_FLOAT

   ! work group size
   integer(c_size_t), parameter :: NXL = 16
   integer(c_size_t) :: nxGlobal, nxLocal

   Type(Context) :: aContext
   Integer       :: ndims, dims(3)
!...TODO-END-GENERATE

   real(c_float), target, dimension(1-NH:NX+NH) :: A, B, C

   type(CPUTimer) :: timer
   real(c_double) :: h_time

   integer :: device, i, nLoops = 1, nWarm = 0

!...TODO-GENERATE
   real(c_float), allocatable :: in_A_H_(:), out_A_H_(:)
   real(c_float), allocatable :: in_B_H_(:), out_B_H_(:)
   real(c_float), allocatable :: in_C_H_(:), out_C_H_(:)

   type(CLDevice) :: cl_device_
   type(CLKernel) :: kernel
   type(CLBuffer) :: cl_A_, cl_B_, cl_C_
   type(CLBuffer) :: cl_A_H_, cl_B_H_, cl_C_H_

   integer(c_size_t) :: mem_size = (NX+2*NH) * SIZE_FLOAT

   allocate(in_A_H_(N_HALO_ELEM), out_A_H_(N_HALO_ELEM))
   allocate(in_B_H_(N_HALO_ELEM), out_B_H_(N_HALO_ELEM))
   allocate(in_C_H_(N_HALO_ELEM), out_C_H_(N_HALO_ELEM))

   Call Parallel_Start(aContext)

   ndims = 1
   dims  = [0,0,0]

   Call Parallel_Topology(aContext, ndims, dims)
!...TODO-END-GENERATE

   device = 1 + aContext%rank
   cl_status_ = init_device(cl_device_, device)

   nxGlobal = NX
   if (device /= 0) then
      nxLocal = NXL
   else
      nxLocal = 1
   end if

   A = 0
   B = 0
   C = 0
   do i = 1, NX
      A(i) = i
      B(i) = i + 100*(1 + aContext%rank)
   end do

!   out_A_H_ = [-1.0*NX, -1.]
!   out_B_H_ = [-100-1.0*NX, -101.]
   out_C_H_ = [-2.0, -1.0, 9.0, 10.0]

!   A_H = [-NX+1.0, -1.0*NX, -1., -2.]
!   B_H = [-100-NX+1.0, -100-1.0*NX, -101., -102.]

   ! create memory buffers
   !
   cl_A_ = createBuffer(cl_device_, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(A))
   cl_B_ = createBuffer(cl_device_, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(B))
   cl_C_ = createBuffer(cl_device_, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(C))
!   cl_C_ = createBuffer(cl_device_, CL_MEM_WRITE_ONLY, mem_size, C_NULL_PTR)

   cl_A_H_ = createBuffer(cl_device_, CL_MEM_READ_WRITE+CL_MEM_COPY_HOST_PTR, HALO_SIZE, c_loc(out_A_H_))
   cl_B_H_ = createBuffer(cl_device_, CL_MEM_READ_WRITE+CL_MEM_COPY_HOST_PTR, HALO_SIZE, c_loc(out_B_H_))
   cl_C_H_ = createBuffer(cl_device_, CL_MEM_READ_WRITE+CL_MEM_COPY_HOST_PTR, HALO_SIZE, c_loc(out_C_H_))

   ! create the kernel
   !
   kernel = createKernel(cl_device_, "elemental_add_1d")

   ! add arguments
   !
   cl_status_ = setKernelArgMem(kernel, 0, clMemObject(cl_A_))
   cl_status_ = setKernelArgMem(kernel, 1, clMemObject(cl_B_))
   cl_status_ = setKernelArgMem(kernel, 2, clMemObject(cl_C_))
   cl_status_ = setKernelArgMem(kernel, 3, clMemObject(cl_A_H_))
   cl_status_ = setKernelArgMem(kernel, 4, clMemObject(cl_B_H_))
   cl_status_ = setKernelArgMem(kernel, 5, clMemObject(cl_C_H_))

   !... warmup
   do i = 1, nWarm
      cl_status_ = run(kernel, nxGlobal, nxLocal)
   end do

   ! run the kernel on the device
   !
   print *
   print *, "Measuring time to compute elemental add..."
   call init(timer)
   call start(timer)

   do i = 1, nLoops
      cl_status_ = writeBuffer(cl_C_H_, c_loc(out_C_H_), HALO_SIZE)
      cl_status_ = run(kernel, nxGlobal, nxLocal)
      cl_status_ = readBuffer(cl_C_H_, c_loc(in_C_H_), HALO_SIZE)
      call Halo_Exchange1D (in_C_H_, out_C_H_, NX, NH)
   end do
   cl_status_ = clFinish(kernel%commands)
   call stop(timer)

   h_time = elapsed_time(timer)
   print *, "   host time ==", real(h_time)/nLoops, "ms per iteration"

   ! get the results
   !
   cl_status_ = readBuffer(cl_A_, c_loc(A), mem_size)
   cl_status_ = readBuffer(cl_B_, c_loc(B), mem_size)
   cl_status_ = readBuffer(cl_C_, c_loc(C), mem_size)
   cl_status_ = readBuffer(cl_A_H_, c_loc(in_A_H_), HALO_SIZE)
   cl_status_ = readBuffer(cl_B_H_, c_loc(in_B_H_), HALO_SIZE)
   cl_status_ = readBuffer(cl_C_H_, c_loc(in_C_H_), HALO_SIZE)

   !... halo transfer
   !
   ! 1. copy halo inside buffers from device
   ! 2. transfer halos
   ! 3. copy halo outside buffers to device

   C(1-NH:0) = -in_C_H_(1+NH:)
   C(NX+1:)  = -in_C_H_(1:NH)

   print *, A(:)
   print *
   print *, B(:)
   print *
   print *, C(:)
   print *
!   print *, in_A_H_(:)
!   print *
!   print *, in_B_H_(:)
!   print *
   print *,  in_C_H_(:)
   print *, out_C_H_(:)
   print *

   do i = 1, NX
      if (C(i) /= A(i) + B(i)) then
         print *, aContext%rank, ": Results incorrect at ", i
         goto 99
      end if
   end do

   if (cl_status_ == CL_SUCCESS) then
      print *, aContext%rank, ": Correctness verified..."
   end if
   print *

!...TODO-GENERATE
99 Call Parallel_End(aContext)
!...TODO-END-GENERATE

end program
