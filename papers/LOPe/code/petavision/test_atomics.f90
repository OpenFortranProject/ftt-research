program test_atomics
   use OpenCL
   implicit none

   integer :: status

   integer(c_int), target :: I

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_I

   integer, parameter :: NXL = 16
   integer, parameter :: NYL = 16
   integer, parameter :: NX = 64
   integer, parameter :: NY = 64

   integer(c_size_t) :: global_mem_size = 4
   integer(c_size_t) :: nxLocal=NXL, nyLocal=NXL
   integer(c_size_t) :: nxGlobal=NX, nyGlobal=NY
   integer :: device_id

   device_id = 0  ! 0=GPU, 1=CPU
   status = init(device, device_id)

   status = query(device)

   call limitLocalSize(device, nxLocal, nyLocal)

   ! initialize memory
   !

   I = 0;

   ! create memory buffers
   !
   d_I = createBuffer(device, CL_MEM_COPY_HOST_PTR, global_mem_size, c_loc(I))

   ! create the kernel
   !
   kernel = createKernel(device, &
                         "test_atomics.cl" // C_NULL_CHAR, &
                         "atomic_inc"      // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgMem(kernel, 0, clMemObject(d_I))

   ! run the kernel on the device
   !

   print *
   status = run(kernel, nxGlobal, nyGlobal, nxLocal, nyLocal)
   status = clFinish(kernel%commands)

   ! get the results
   !
   status = readBuffer(d_I, c_loc(I), global_mem_size)
   
   print *, "results==", I

end program test_atomics
