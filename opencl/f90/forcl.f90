program forcl
   use OpenCL
   implicit none
   integer :: status

   integer(c_size_t), parameter :: NX  = 64
   integer(c_size_t), parameter :: NY  = 64
   integer(c_size_t), parameter :: NXL = 16
   integer(c_size_t), parameter :: NYL = 16
   integer,           parameter :: NPAD = 1

   integer(c_size_t), parameter :: SIZE_INT = 4

   integer, target, dimension(NX+2*NPAD,NY+2*NPAD) :: A, B, C

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_A, d_B, d_C

   integer(c_size_t) :: nxLocal, nyLocal
   integer(c_size_t) :: global_ex_mem_size = (NX +2*NPAD)*(NY +2*NPAD) * SIZE_INT
   integer(c_size_t) :: local_ex_mem_size  = (NXL+2*NPAD)*(NYL+2*NPAD) * SIZE_INT

   integer :: device_id

   if (NXL < 2*NPAD .or. NYL < 2*NPAD) then
      print *, "thread work group size is too small, die!!!"
      stop 1
   end if

   device_id = 0
   if (device_id == 0) then
      nxLocal = NXL; nyLocal = NYL
   else
      nxLocal = 1; nyLocal = 1
   end if

   status = init(device, device_id)

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
                         "shift_individually" // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgInt(kernel, 0, NPAD)
   status = setKernelArgMem(kernel, 1, clMemObject(d_A))
   status = setKernelArgMem(kernel, 2, clMemObject(d_B))
   status = setKernelArgMem(kernel, 3, clMemObject(d_C))
   status = setKernelArgLoc(kernel, 4, local_ex_mem_size)

   ! run the kernel on the device
   !
   status = run(kernel, NX, NY, nxLocal, nyLocal)

   ! get the results
   !
   status = readBuffer(d_C, c_loc(C), global_ex_mem_size)

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

end program
