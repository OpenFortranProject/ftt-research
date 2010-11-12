elemental function update_weight_decr(nPad, dt, apost, m) result(decr)
   integer, intent(in) :: nPad
   real, intent(in) :: dt, apost, m
   real :: decayLTD, decr
   real, parameter :: ampLTD = 1.1, tauLTD = 20.0

   decayLTD = exp(-dt / tauLTD)
   decr = decayLTD*M - ampLTD*APost
   
end function update_weight_decr   


program update_weights
   use OpenCL
   use Timer_mod
   implicit none

   interface
      elemental function update_weight_decr(nPad, dt, apost, m) result(decr)
         implicit none
         integer, intent(in) :: nPad
         real, intent(in) :: dt, apost, m
         real :: decr
      end function update_weight_decr   
      subroutine update_weight_c(n, dt, APost, M) bind(C,name="update_weight_c")
         use ISO_C_BINDING
         implicit none
         integer(c_size_t), value :: n
         real(c_float), value :: dt
         real(c_float), dimension(*) :: aPost, M
      end subroutine
   end interface

   integer :: status

   ! layer size
   integer(c_size_t), parameter :: NX  = 512
   integer(c_size_t), parameter :: NY  = 512
   integer(c_size_t), parameter :: NF  = 1
   integer,           parameter :: NPAD = 0

   integer,           parameter :: nxScale = 4
   integer,           parameter :: nyScale = 4
  
   ! patch size
   integer(c_int), parameter :: NXP = 8
   integer(c_int), parameter :: NYP = 8

   ! work group size
   integer(c_size_t), parameter :: NXL = 16
   integer(c_size_t), parameter :: NYL = 4
   integer(c_size_t) :: nxLocal, nyLocal

   integer(c_size_t), parameter :: SIZE_FLOAT = 4

   real(c_float), target, dimension(NX*nxScale+2*NPAD,NY*nyScale+2*NPAD) :: APost, M
   real(c_float), target, dimension(NXP*NYP,NX,NY) :: P, W
   real(c_float) :: dt

   type(CPUTimer) :: timer
   integer(c_int64_t) :: time

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_APost, d_M, d_P, d_W

   integer(cl_bitfield) :: flags
   integer(c_size_t) :: global_mem_size = NX*NY * SIZE_FLOAT
   integer(c_size_t) :: local_mem_size  = NXL*NYL * SIZE_FLOAT
   integer(c_size_t) :: global_mem_ex_size = (NX +2*NPAD)*(NY +2*NPAD) * SIZE_FLOAT
   integer(c_size_t) :: local_mem_ex_size  = (NXL+2*NPAD)*(NYL+2*NPAD) * SIZE_FLOAT

   integer :: device_id, i, nLoops
   integer :: ocl_time = 0
   real :: bandwidth

   dt = 0.5

   device_id = 0
   nLoops = 20

   if (device_id == 0) then
      nxLocal = NXL; nyLocal = NYL
   else
      nxLocal = 1; nyLocal = 1
   end if

   status = init(device, device_id)

   ! initialize buffers
   !
   APost = 1.1
   M = 0.01
   P = 0.02
   W = 1.0 / (NXP*NYP)

   ! create memory buffers
   !
   d_APost = createBuffer(device, CL_MEM_COPY_HOST_PTR, &
                          global_mem_ex_size*nxScale*nyScale, c_loc(APost))
   d_M     = createBuffer(device, CL_MEM_COPY_HOST_PTR, &
                          global_mem_ex_size*nxScale*nyScale, c_loc(M))

   d_P = createBuffer(device, CL_MEM_COPY_HOST_PTR, global_mem_size*NXP*NYP, c_loc(P))
   d_W = createBuffer(device, CL_MEM_COPY_HOST_PTR, global_mem_size*NXP*NYP, c_loc(W))

   ! create the kernel
   !
   kernel = createKernel(device, &
                         "update_weights.cl"  // C_NULL_CHAR, &
                         "update_weight_decr" // C_NULL_CHAR)

   ! add arguments for update_weights kernel
   !
   status = setKernelArgInt (kernel, 0, NPAD) + status
   status = setKernelArgReal(kernel, 1, dt) + status
   status = setKernelArgMem (kernel, 2, clMemObject(d_APost)) + status
   status = setKernelArgMem (kernel, 3, clMemObject(d_M)) + status
   status = setKernelArgLoc (kernel, 4, local_mem_size) + status

   ! add arguments for update_weights kernel
   !
!   status = kernel%setKernelArgInt(0, NPAD) + status
!   status = kernel%setKernelArgInt(1, NXP) + status
!   status = kernel%setKernelArgInt(2, NYP) + status
!   status = kernel%setKernelArgInt(3, NFP) + status
!   status = kernel%setKernelArgMem(4, d_A%clMemObject()) + status
!   status = kernel%setKernelArgMem(5, d_M%clMemObject()) + status
!   status = kernel%setKernelArgMem(6, d_P%clMemObject()) + status
!   status = kernel%setKernelArgMem(7, d_W%clMemObject()) + status

   ! run the kernel on the device
   !
   print *
   call init_timer(timer)
   call start(timer)
   do i = 0, nLoops
      status = run(kernel, NX*nxScale, NY*nyScale, nxLocal, nyLocal) + status
      if (i > 0) then
         ocl_time = ocl_time + kernel%elapsed
      end if
   end do
   call stop(timer)
   call print_elapsed_time(timer)
   print *, "opencl timer==", ocl_time/1000, "ms"

   ! 1.0e-9 -> GB, 1000 -> ms, 3 -> 2*to/1*fro
   bandwidth = (1.0e-9 * 1000) * nLoops * (3*global_mem_ex_size*nxScale*nyScale/(ocl_time/1000))
   print *, "bandwidth ==", bandwidth, "GB/s"

   ! get the results
   !
   status = readBuffer(d_W, c_loc(W), global_mem_size) + status

   if (status /= CL_SUCCESS) print *, "status=", status

   print *
   call init_timer(timer)
   call start(timer)
   do i = 1, nLoops
      M = update_weight_decr(1, dt, APost, M)
   end do
   call stop(timer)
   call print_elapsed_time(timer)

   call init_timer(timer)
   call start(timer)
   do i = 1, nLoops
      call update_weight_c(NX*nxScale * NY*nyScale, dt, APost, M)
   end do
   call stop(timer)
   call print_elapsed_time(timer)

end program
