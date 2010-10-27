program forcl
   use OpenCL
   integer :: status
   integer, parameter :: NX = 16
   integer, parameter :: NY = 16

   integer(c_size_t) :: nxg, nyg, nxl, nyl
   integer, target, dimension(NX,NY) :: A, B, C
   integer, pointer, dimension(:,:) :: p_A, p_B, p_C

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_A, d_B, d_C
   type(c_ptr)    :: h_A, h_B, h_C

   integer(cl_bitfield) :: flags
   integer(c_size_t) :: byte_size = 4*NX*NY

   status = device%init(1)

   ! create memory buffers
   !
   d_A = device%createBuffer(byte_size, c_loc(A))
   d_B = device%createBuffer(byte_size, c_loc(B))
   d_C = device%createBuffer(byte_size, c_loc(C))

   ! map memory so that it can be initialized on host
   !
   h_A = d_A%map(CL_MAP_WRITE)
   h_B = d_B%map(CL_MAP_WRITE)

   call c_f_pointer(h_A, p_A, shape(A))
   call c_f_pointer(h_B, p_B, shape(B))

   p_A = 1
   p_B = 2

   ! finished initializing memory, unmap for use on device
   !
   status = d_A%unmap()
   status = d_B%unmap()

   ! create the kernel
   !
   kernel = device%createKernel("elemental_add.cl", "elemental_add")

   ! add arguments
   !
   status = kernel%setKernelArg(0, d_A%clMemObject()) + status
   status = kernel%setKernelArg(1, d_B%clMemObject()) + status
   status = kernel%setKernelArg(2, d_C%clMemObject()) + status

   nxg = NX; nyg = NY;
   nxl = 1;  nyl = 1;
   status = kernel%run(nxg, nyg, nxl, nyl) + status

   ! get the results
   !
   h_C = d_C%map(CL_MAP_READ)
   call c_f_pointer(h_C, p_C, shape(C))

   print *, p_C(3,3), " =", p_A(3,3), " +", p_B(3,3)

   if (status /= CL_SUCCESS) print *, "status=", status

end program
