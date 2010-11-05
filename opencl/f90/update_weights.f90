elemental function update_weight_decr(nPad, dt, APost, M) result(decr)
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

   interface
      subroutine update_weight_c(n, dt, APost, M) bind(C,name="update_weight_c")
         use ISO_C_BINDING
         integer(c_size_t), value :: n
         real, value :: dt
         real, dimension(*) :: aPost, M
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
   integer(c_size_t), parameter :: NYL = 16
   integer(c_size_t) :: nxg, nyg

   integer(c_size_t), parameter :: SIZE_FLOAT = 4

   real(c_float), target, dimension(NX*nxScale+2*NPAD,NY*nyScale+2*NPAD) :: APost, M
   real(c_float), target, dimension(NXP*NYP,NX,NY) :: P, W
   real(c_float), pointer, dimension(:,:)   :: p_APost, p_M
   real(c_float), pointer, dimension(:,:,:) :: p_P, p_W

   type(MachTimer) :: timer
   integer(c_int64_t) :: time

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_APost, d_M, d_P, d_W
   type(c_ptr)    :: h_APost, h_M, h_P, h_W

   integer(cl_bitfield) :: flags
   integer(c_size_t) :: global_size = NX*NY * SIZE_FLOAT
   integer(c_size_t) :: local_size  = NXL*(NYL+2*NPAD) * SIZE_FLOAT
   integer(c_size_t) :: global_ex_size = (NX +2*NPAD)*(NY +2*NPAD) * SIZE_FLOAT
   integer(c_size_t) :: local_ex_size  = (NXL+2*NPAD)*(NYL+2*NPAD) * SIZE_FLOAT

   integer :: device_id, i

   device_id = 0
   if (device_id == 0) then
      nxg = NXL; nyg = NYL
   else
      nxg = 1; nyg = 1
   end if

   status = device%init(device_id)

   ! create memory buffers
   !
   d_APost = device%createBuffer(global_ex_size*nxScale*nyScale, c_loc(APost))
   d_M     = device%createBuffer(global_ex_size*nxScale*nyScale, c_loc(M))

   d_P = device%createBuffer(global_size*NXP*NYP, c_loc(P))
   d_W = device%createBuffer(global_size*NXP*NYP, c_loc(W))

   ! map memory so that it can be initialized on host
   !
   h_APost = d_APost%map(CL_MAP_WRITE)
   h_M = d_M%map(CL_MAP_WRITE)
   h_P = d_P%map(CL_MAP_WRITE)
   h_W = d_W%map(CL_MAP_WRITE)

   call c_f_pointer(h_APost, p_APost, shape(APost))
   call c_f_pointer(h_M, p_M, shape(M))
   call c_f_pointer(h_P, p_P, shape(P))
   call c_f_pointer(h_W, p_W, shape(W))

   p_A = 1.0
   p_M = 0.01
   p_P = 0.02
   p_W = 1.0 / (NXP*NYP)

   ! finished initializing memory, unmap for use on device
   !
   status = d_APost%unmap()
   status = d_M%unmap()
   status = d_P%unmap()
   status = d_W%unmap()

   ! create the kernel
   !
   kernel = device%createKernel("update_weights.cl", "update_weight_decr")

   ! add arguments for update_weights kernel
   !
   status = kernel%setKernelArgInt (0, NPAD) + status
   status = kernel%setKernelArgReal(1, dt) + status
   status = kernel%setKernelArgMem (2, d_APost%clMemObject()) + status
   status = kernel%setKernelArgMem (3, d_M%clMemObject()) + status

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
   call timer%init()
   call timer%start()
   do i = 1, 10
      status = kernel%run(NX*nxScale, NY*nyScale, nxg, nyg) + status
   end do
   call timer%stop()
   call timer%elapsed_time()

   ! get the results
   !
   h_W = d_W%map(CL_MAP_READ)
   call c_f_pointer(h_W, p_W, shape(W))

   if (status /= CL_SUCCESS) print *, "status=", status

   print *
   call timer%init()
   call timer%start()
   do i = 1, 10
      M = update_weight_decr(1, .5, APost, M)
   end do
   call timer%stop()
   call timer%elapsed_time()

   print *
   call timer%init()
   call timer%start()
   do i = 1, 10
      call update_weight_c(NX*nxScale * NY*nyScale, .5, APost, M)
   end do
   call timer%stop()
   call timer%elapsed_time()

end program
