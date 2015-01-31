program elemental_add_3d
!...TODO-GENERATE
   use ForOpenCL
   use Parallel_Halo , only : Context, Parallel_Start, Parallel_End, Parallel_Topology
   use Parallel      , only : Halo_Exchange3D
!...TODO-END-GENERATE
   use Timer_mod
   implicit none

   ! layer size
   integer(c_size_t), parameter :: NX  = 16
   integer(c_size_t), parameter :: NY  = 4
   integer(c_size_t), parameter :: NZ  = 4

!...TODO-GENERATE
   integer :: cl_status_

   integer(c_size_t), parameter :: NHX = 1
   integer(c_size_t), parameter :: NHY = 1
   integer(c_size_t), parameter :: NHZ = 1

   integer(c_size_t), parameter ::  SIZE_FLOAT = 4
   integer(c_size_t), parameter :: N_HALO_ELEM = (2*NHX)*NY*NZ + NX*(2*NHY)*NZ + NX*NY*(2*NHZ)
   integer(c_size_t), parameter ::   HALO_SIZE = N_HALO_ELEM*SIZE_FLOAT

   ! work group size
   integer(c_size_t), parameter :: NXL = 32
   integer(c_size_t), parameter :: NYL = 8
   integer(c_size_t), parameter :: NZL = 8
   integer(c_size_t) :: nxGlobal, nyGlobal, nzGlobal, nxLocal, nyLocal, nzLocal

   Type(Context) :: aContext
   Integer       :: ndims, dims(3)
!...TODO-END-GENERATE

   real(c_float), target, allocatable, dimension(:,:,:) :: A, B, C

   type(CPUTimer) :: timer
   real(c_double) :: h_time

   integer :: device, i, j, k, nLoops = 1, nWarm = 1

!...TODO-GENERATE
   real(c_float), allocatable :: in_C_H_(:), out_C_H_(:)

   type(CLDevice) :: cl_device_
   type(CLKernel) :: kernel
   type(CLBuffer) :: cl_A_, cl_B_, cl_C_
   type(CLBuffer) :: cl_C_H_

   integer(c_size_t) :: mem_size = (NX+2*NHX) * (NY+2*NHY) * (NZ+2*NHZ) * SIZE_FLOAT

   allocate(in_C_H_(N_HALO_ELEM), out_C_H_(N_HALO_ELEM))

   Call Parallel_Start(aContext)

   ndims = 1
   dims  = [0,0,0]

   Call Parallel_Topology(aContext, ndims, dims)
!...TODO-END-GENERATE

   device = 1

   nxGlobal = NX; nyGlobal = NY; nzGlobal = NZ
   if (device /= 0) then
      nxLocal = NXL; nyLocal = NYL; nzLocal = NZL
   else
      nxLocal = 1; nyLocal = 1; nzLocal = 1
   end if

   cl_status_ = init_device(cl_device_, device)

   allocate(A(1-NHX:NX+NHX,1-NHY:NY+NHY,1-NHZ:NZ+NHZ))
   allocate(B(1-NHX:NX+NHX,1-NHY:NY+NHY,1-NHZ:NZ+NHZ))
   allocate(C(1-NHX:NX+NHX,1-NHY:NY+NHY,1-NHZ:NZ+NHZ))

   A = 0
   B = 1
   do k = 1, NZ
      do j = 1, NY
         do i = 1, NX
            A(i,j,k) = i + 100*j + 1000*k
         end do
      end do
   end do

   ! create memory buffers
   !
   cl_A_ = createBuffer(cl_device_, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(A))
   cl_B_ = createBuffer(cl_device_, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(B))
   cl_C_ = createBuffer(cl_device_, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(C))
!   cl_C_ = createBuffer(cl_device_, CL_MEM_WRITE_ONLY, mem_size, C_NULL_PTR)

   cl_C_H_ = createBuffer(cl_device_, CL_MEM_READ_WRITE+CL_MEM_COPY_HOST_PTR, HALO_SIZE, c_loc(out_C_H_))

   ! create the kernel
   !
   kernel = createKernel(cl_device_, "elemental_add_3d")

   ! add arguments
   !
   cl_status_ = setKernelArgMem(kernel, 0, clMemObject(cl_A_))
   cl_status_ = setKernelArgMem(kernel, 1, clMemObject(cl_B_))
   cl_status_ = setKernelArgMem(kernel, 2, clMemObject(cl_C_))
   cl_status_ = setKernelArgMem(kernel, 3, clMemObject(cl_C_H_))

   ! run the kernel on the device
   !
   print *
   print "('Measuring time to compute elemental add (NX,NY,NZ):', 3I6)", NX, NY, NZ
   call init(timer)
   call start(timer)
   do i = 1, nLoops
      cl_status_ = writeBuffer(cl_C_H_, c_loc(out_C_H_), HALO_SIZE)
!      cl_status_ = run(kernel, nxGlobal, nyGlobal, nxLocal, nyLocal)
      cl_status_ = readBuffer(cl_C_H_, c_loc(in_C_H_), HALO_SIZE)
      call Halo_Exchange3D (in_C_H_, out_C_H_, NX,NHX, NY,NHY, NZ,NHZ)
   end do
   cl_status_ = clFinish(kernel%commands)
   call stop(timer)

   h_time = elapsed_time(timer)
   print *, "   host time ==", real(h_time)/nLoops, "ms per iteration"

   ! get the results
   !
   cl_status_ = readBuffer(cl_C_, c_loc(C), mem_size)
   cl_status_ = readBuffer(cl_C_H_, c_loc(in_C_H_), HALO_SIZE)

!   print *
!   print *, A(:,4)
!   print *
!   print *, B(:,4)
!   print *
!   print *, C(:,4)
!   print *
!   print *, in_C_H_(1:NX)
!   print *
!   print *, in_C_H_(NX+1:2*NX)
!   print *
!   print *, in_C_H_(2*NX+1:2*NX+NY)
!   print *
!   print *, in_C_H_(2*NX+1+NY:2*NX+2*NY)
!   print *

   do k = 1, nz
      do j = 1, ny
         do i = 1, nx
            if (C(i,j,k) /= A(i,j,k) + B(i,j,k)) then
               print *, "Results incorrect at ", i, j, k
               stop 1
            end if
         end do
      end do
   end do

   if (cl_status_ == CL_SUCCESS) then
      print *, "Correctness verified..."
   end if
   print *

   deallocate(A, B, C)

!...TODO-GENERATE
   Call Parallel_End(aContext)
!...TODO-END-GENERATE

end program
