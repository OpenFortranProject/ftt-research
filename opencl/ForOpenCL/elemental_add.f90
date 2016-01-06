program elemental_add
   use ForOpenCL
   use Timer_mod
   implicit none

   integer :: status

   ! layer size
   integer(c_size_t), parameter :: NX  = 2048
   integer(c_size_t), parameter :: NY  = 2048
  
   ! work group size
   integer(c_size_t), parameter :: NXL = 16
   integer(c_size_t), parameter :: NYL = 8
   integer(c_size_t) :: nxLocal, nyLocal
   integer(c_int) :: nxg, nyg

   integer(c_size_t), parameter :: SIZE_FLOAT = 4

   real(c_float), target, dimension(NX,NY) :: A, B, C

   type(CPUTimer) :: timer
   real(c_double) :: h_time

   type(CLDevice) :: device
   type(CLKernel) :: kernel1, kernel2
   type(CLBuffer) :: d_A, d_B, d_C

   integer(c_size_t) :: mem_size = NX*NY * SIZE_FLOAT

   integer :: device_id, i, j, nLoops = 100

   device_id = 1
   status = init_device(device, device_id)
   status = query(device)

   nxg = NX
   nyg = NY
   if (device_id /= 0) then
      nxLocal = NXL; nyLocal = NYL
   else
      nxLocal = 1; nyLocal = 1
   end if

   A = 1
   B = 2

   ! create memory buffers
   !
   d_A = createBuffer(device, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(A))
   d_B = createBuffer(device, CL_MEM_READ_ONLY + CL_MEM_COPY_HOST_PTR, mem_size, c_loc(B))
   d_C = createBuffer(device, CL_MEM_WRITE_ONLY, mem_size, C_NULL_PTR)

   ! create the kernel
   !
   kernel1 = createKernel(device, "elemental_add", "elementals.cl")
   kernel2 = createKernel(device, "elemental_sub", "elementals.cl")

   ! add arguments
   !
   status = setKernelArgMem(kernel1, 0, clMemObject(d_A)) + status
   status = setKernelArgMem(kernel1, 1, clMemObject(d_B)) + status
   status = setKernelArgMem(kernel1, 2, clMemObject(d_C)) + status

   status = setKernelArgMem(kernel2, 0, clMemObject(d_A)) + status
   status = setKernelArgMem(kernel2, 1, clMemObject(d_B)) + status
   status = setKernelArgMem(kernel2, 2, clMemObject(d_C)) + status

   ! run the kernel on the device
   !
   print *
   print *, "Measuring time to compute elemental add..."
   call init(timer)
!   call start(timer)
   do i = 1, nLoops
      status = run(kernel1, NX, NY, nxLocal, nyLocal) + status
   end do
   status = clFinish(kernel1%commands)
   call stop(timer)

   h_time = elapsed_time(timer)
   print *, "   host time ==", real(h_time)/nLoops, "ms per iteration"

   ! get the results
   !
   status = readBuffer(d_C, c_loc(C), mem_size) + status

   do j = 1, ny
      do i = 1, nx
         if (C(i,j) /= A(i,j) + B(i,j)) then
            print *, "Results incorrect at ", i, j
            stop 1
         end if
      end do
   end do

   if (status == CL_SUCCESS) then
      print *, "Correctness verified for elemental_add..."
   end if

   status = run(kernel2, NX, NY, nxLocal, nyLocal)
   status = clFinish(kernel2%commands)
   status = readBuffer(d_C, c_loc(C), mem_size) + status

   do j = 1, ny
      do i = 1, nx
         if (C(i,j) /= A(i,j) - B(i,j)) then
            print *, "Results incorrect at ", i, j
            stop 1
         end if
      end do
   end do

   if (status == CL_SUCCESS) then
      print *, "Correctness verified for elemental_sub..."
   end if
   print *

end program
