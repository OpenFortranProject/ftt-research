program elemental_add_1d
   use ForOpenCL
   use Timer_mod
   implicit none

   integer :: status

   ! layer size
   integer(c_size_t), parameter :: NX  = 16
   integer(c_size_t), parameter :: NH  = 1
  
   ! work group size
   integer(c_size_t), parameter :: NXL = 16
   integer(c_size_t) :: nxLocal
   integer(c_int) :: nxg

   integer(c_size_t), parameter :: SIZE_FLOAT = 4

   real(c_float), target, dimension(1-NH:NX+NH) :: A, B, C
   real(c_float), target, dimension(2*NH)   :: A_H, B_H, C_H

   type(CPUTimer) :: timer
   real(c_double) :: h_time

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: cl_A_, cl_B_, cl_C_
   type(CLBuffer) :: cl_A_H_, cl_B_H_, cl_C_H_

   integer(c_size_t) :: mem_size = (NX+2*NH) * SIZE_FLOAT

   integer :: device_id, i, nLoops = 100

   device_id = 1

   nxg = NX
   if (device_id /= 0) then
      nxLocal = NXL
   else
      nxLocal = 1
   end if

   status = init_device(device, device_id)

   A = 0
   B = 0
   C = 0
   do i = 1, NX
      A(i) = i
      B(i) = i + 100
   end do

   A_H = [-1.0*NX, -1.]
   B_H = [-100-1.0*NX, -101.]
!   A_H = [-NX+1.0, -1.0*NX, -1., -2.]
!   B_H = [-100-NX+1.0, -100-1.0*NX, -101., -102.]

   ! create memory buffers
   !
   cl_A_ = createBuffer(device, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(A))
   cl_B_ = createBuffer(device, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(B))
   cl_C_ = createBuffer(device, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(C))
!   cl_C_ = createBuffer(device, CL_MEM_WRITE_ONLY, mem_size, C_NULL_PTR)

   cl_A_H_ = createBuffer(device, CL_MEM_READ_WRITE+CL_MEM_COPY_HOST_PTR, 2*NH*SIZE_FLOAT, c_loc(A_H))
   cl_B_H_ = createBuffer(device, CL_MEM_READ_WRITE+CL_MEM_COPY_HOST_PTR, 2*NH*SIZE_FLOAT, c_loc(B_H))
   cl_C_H_ = createBuffer(device, CL_MEM_READ_WRITE+CL_MEM_COPY_HOST_PTR, 2*NH*SIZE_FLOAT, c_loc(C_H))

   ! create the kernel
   !
   kernel = createKernel(device, "elemental_add_1d")

   ! add arguments
   !
   status = setKernelArgMem(kernel, 0, clMemObject(cl_A_))
   status = setKernelArgMem(kernel, 1, clMemObject(cl_B_))
   status = setKernelArgMem(kernel, 2, clMemObject(cl_C_))
   status = setKernelArgMem(kernel, 3, clMemObject(cl_A_H_))
   status = setKernelArgMem(kernel, 4, clMemObject(cl_B_H_))
   status = setKernelArgMem(kernel, 5, clMemObject(cl_C_H_))

   ! run the kernel on the device
   !
   print *
   print *, "Measuring time to compute elemental add..."
   call init(timer)
   call start(timer)
   do i = 1, nLoops
      status = run(kernel, nxLocal, nxLocal)
   end do
   status = clFinish(kernel%commands)
   call stop(timer)

   h_time = elapsed_time(timer)
   print *, "   host time ==", real(h_time)/nLoops, "ms per iteration"

   ! get the results
   !
   status = readBuffer(cl_A_, c_loc(A), mem_size)
   status = readBuffer(cl_B_, c_loc(B), mem_size)
   status = readBuffer(cl_C_, c_loc(C), mem_size)
   status = readBuffer(cl_A_H_, c_loc(A_H), 2*NH*SIZE_FLOAT)
   status = readBuffer(cl_B_H_, c_loc(B_H), 2*NH*SIZE_FLOAT)
   status = readBuffer(cl_C_H_, c_loc(C_H), 2*NH*SIZE_FLOAT)

   C(1-NH:0) = -C_H(1+NH:)
   C(NX+1:)  = -C_H(1:NH)

   print *, A(:)
   print *
   print *, B(:)
   print *
   print *, C(:)
   print *
   print *, A_H(:)
   print *
   print *, B_H(:)
   print *
   print *, C_H(:)
   print *

   do i = 1, NX
      if (C(i) /= A(i) + B(i)) then
         print *, "Results incorrect at ", i
         stop 1
      end if
   end do

   if (status == CL_SUCCESS) then
      print *, "Correctness verified..."
   end if
   print *

end program
