program elemental_add_2d
   use ForOpenCL
   use Timer_mod
   implicit none

   integer :: status

   ! layer size
   integer(c_size_t), parameter :: NX  = 16
   integer(c_size_t), parameter :: NY  = 4
   integer(c_size_t), parameter :: NH  = 1

   integer(c_size_t), parameter :: HALO_SIZE = NX*(NH+NH) + NY*(NH+NH)
  
   ! work group size
   integer(c_size_t), parameter :: NXL = 16
   integer(c_size_t), parameter :: NYL = 8
   integer(c_size_t) :: nxLocal, nyLocal
   integer(c_int) :: nxg, nyg

   integer(c_size_t), parameter :: SIZE_FLOAT = 4

   real(c_float), target, dimension(NX,NY) :: A, B, C
   type(CLBuffer) :: cl_A_, cl_B_, cl_C_

   real(c_float), target, dimension(HALO_SIZE) :: A_H, B_H, C_H
   type(CLBuffer) :: cl_A_H_, cl_B_H_, cl_C_H_

   type(CPUTimer) :: timer
   real(c_double) :: h_time

   type(CLDevice) :: device
   type(CLKernel) :: kernel

   integer(c_size_t) :: mem_size = NX*NY * SIZE_FLOAT

   integer :: device_id, i, j, nLoops = 100

   device_id = 0

   nxg = NX
   nyg = NY
   if (device_id /= 0) then
      nxLocal = NXL; nyLocal = NYL
   else
      nxLocal = 1; nyLocal = 1
   end if

   status = init_device(device, device_id)

   A = 0
   B = 1
   do j = 1, NY
      do i = 1, NX
         A(i,j) = i + 100*j - 1
      end do
   end do

   ! create memory buffers
   !
   cl_A_ = createBuffer(device, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(A))
   cl_B_ = createBuffer(device, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(B))
   cl_C_ = createBuffer(device, CL_MEM_WRITE_ONLY, mem_size, C_NULL_PTR)

   cl_C_H_ = createBuffer(device, CL_MEM_READ_WRITE+CL_MEM_COPY_HOST_PTR &
                                , HALO_SIZE*SIZE_FLOAT, c_loc(C_H))

   ! create the kernel
   !
   kernel = createKernel(device, "elemental_add_2d")

   ! add arguments
   !
   status = setKernelArgMem(kernel, 0, clMemObject(cl_A_))
   status = setKernelArgMem(kernel, 1, clMemObject(cl_B_))
   status = setKernelArgMem(kernel, 2, clMemObject(cl_C_))
   status = setKernelArgMem(kernel, 3, clMemObject(cl_C_H_))

   ! run the kernel on the device
   !
   print *
   print *, "Measuring time to compute elemental add..."
   call init(timer)
!   call start(timer)
   do i = 1, nLoops
      status = run(kernel, NX, NY, nxLocal, nyLocal) + status
   end do
   status = clFinish(kernel%commands)
   call stop(timer)

   h_time = elapsed_time(timer)
   print *, "   host time ==", real(h_time)/nLoops, "ms per iteration"

   ! get the results
   !
   status = readBuffer(cl_C_, c_loc(C), mem_size)
   status = readBuffer(cl_C_H_, c_loc(C_H), HALO_SIZE*SIZE_FLOAT)

   print *
   print *, A(:,1)
   print *
   print *, B(:,1)
   print *
   print *, C(:,1)
   print *
   print *, C_H(1:NX)
   print *
   print *, C_H(NX:2*NX)
   print *

   do j = 1, ny
      do i = 1, nx
         if (C(i,j) /= A(i,j) + B(i,j)) then
            print *, "Results incorrect at ", i, j
            stop 1
         end if
      end do
   end do

   if (status == CL_SUCCESS) then
      print *, "Correctness verified..."
   end if
   print *

end program
