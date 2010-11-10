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

   integer(c_size_t) :: nxLocal, nyLocal
   integer, target, dimension(NX+2*NPAD,NY+2*NPAD) :: A, B, C
   integer, pointer, dimension(:,:) :: p_A, p_B, p_C

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_A, d_B, d_C
   type(c_ptr)    :: h_A, h_B, h_C

   integer(cl_bitfield) :: flags
   integer(c_size_t) :: global_ex_size = (NX +2*NPAD)*(NY +2*NPAD) * SIZE_INT
   integer(c_size_t) :: local_ex_size  = (NXL+2*NPAD)*(NYL+2*NPAD) * SIZE_INT

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

   ! create memory buffers
   !
   d_A = createBufferMapped(device, global_ex_size, c_loc(A))
   d_B = createBufferMapped(device, global_ex_size, c_loc(B))
   d_C = createBufferMapped(device, global_ex_size, c_loc(C))

   ! map memory so that it can be initialized on host
   !
   h_A = map(d_A, CL_MAP_WRITE)
   h_B = map(d_B, CL_MAP_WRITE)

   call c_f_pointer(h_A, p_A, shape(A))
   call c_f_pointer(h_B, p_B, shape(B))

   p_A = 0
   p_B = 0

   p_A(2:NX+NPAD, 2:NY+NPAD) = 1
   p_B(2:NX+NPAD, 2:NY+NPAD) = 2

   ! finished initializing memory, unmap for use on device
   !
   status = unmap(d_A)
   status = unmap(d_B)

   ! create the kernel
   !
   kernel = createKernel(device, &
                         "shift.cl" // C_NULL_CHAR, &
                         "shift"    // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgInt(kernel, 0, NPAD) + status
   status = setKernelArgMem(kernel, 1, clMemObject(d_A)) + status
   status = setKernelArgMem(kernel, 2, clMemObject(d_B)) + status
   status = setKernelArgMem(kernel, 3, clMemObject(d_C)) + status
   status = setKernelArgLoc(kernel, 4, local_ex_size) + status

   ! run the kernel on the device
   !
   status = run(kernel, NX, NY, nxLocal, nyLocal) + status

   ! get the results
   !
   h_C = map(d_C, CL_MAP_READ)
   call c_f_pointer(h_C, p_C, shape(C))

   print *, "external corners"
   print *, p_C(1,1), " =", p_A(1,1), " +", p_B(1,1)
   print *, p_C(1,NX+2*NPAD), " =", p_A(1,NX+2*NPAD), " +", p_B(1,NX+2*NPAD)
   print *, p_C(NX+2*NPAD,1), " =", p_A(NX+2*NPAD,1), " +", p_B(NX+2*NPAD,1)
   print *, p_C(NX+2*NPAD,NX+2*NPAD), " =", p_A(NX+2*NPAD,NX+2*NPAD), " +", p_B(NX+2*NPAD,NX+2*NPAD)

   print *, "internal corners"
   print *, p_C(1+NPAD,1+NPAD), " =", p_A(1+NPAD,1+NPAD), " +", p_B(1+NPAD,1+NPAD)
   print *, p_C(1+NPAD,NX+1*NPAD), " =", p_A(1+NPAD,NX+1*NPAD), " +", p_B(1+NPAD,NX+1*NPAD)
   print *, p_C(NX+1*NPAD,1+NPAD), " =", p_A(NX+1*NPAD,1+NPAD), " +", p_B(NX+1*NPAD,1+NPAD)
   print *, p_C(NX+1*NPAD,NX+1*NPAD), " =", p_A(NX+1*NPAD,NX+1*NPAD), " +", p_B(NX+1*NPAD,NX+1*NPAD)

   print *, "partial corners"
   print *, p_C(1+NPAD,2+NPAD), " =", p_A(1+NPAD,2+NPAD), " +", p_B(1+NPAD,2+NPAD)
   print *, p_C(1+NPAD,NX+0*NPAD), " =", p_A(1+NPAD,NX+0*NPAD), " +", p_B(1+NPAD,NX+0*NPAD)
   print *, p_C(NX+1*NPAD,2+NPAD), " =", p_A(NX+1*NPAD,2+NPAD), " +", p_B(NX+1*NPAD,2+NPAD)
   print *, p_C(NX+1*NPAD,NX+0*NPAD), " =", p_A(NX+1*NPAD,NX+0*NPAD), " +", p_B(NX+1*NPAD,NX+0*NPAD)

   print *, "internal-1 corners"
   print *, p_C(2+NPAD,2+NPAD), " =", p_A(2+NPAD,2+NPAD), " +", p_B(2+NPAD,2+NPAD)
   print *, p_C(2+NPAD,NX+0*NPAD), " =", p_A(2+NPAD,NX+0*NPAD), " +", p_B(2+NPAD,NX+0*NPAD)
   print *, p_C(NX+0*NPAD,2+NPAD), " =", p_A(NX+0*NPAD,2+NPAD), " +", p_B(NX+0*NPAD,2+NPAD)
   print *, p_C(NX+0*NPAD,NX+0*NPAD), " =", p_A(NX+0*NPAD,NX+0*NPAD), " +", p_B(NX+0*NPAD,NX+0*NPAD)

   print *, "interior points"
   print *, p_C(5,5), " =", p_A(5,5), " +", p_B(5,5)
   print *, p_C(48,48), " =", p_A(48,48), " +", p_B(48,48)

   if (status /= CL_SUCCESS) print *, "status=", status

end program
