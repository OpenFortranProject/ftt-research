module OpenCLTypes
  use, intrinsic :: ISO_C_BINDING
  use Timer_mod

  integer, parameter :: cl_int      = C_INT
  integer, parameter :: cl_uint     = C_INT
  integer, parameter :: cl_ulong    = C_LONG
  integer, parameter :: cl_bool     = cl_int
  integer, parameter :: cl_bitfield = cl_ulong

  integer, parameter :: cl_program_build_info = cl_uint
  integer, parameter :: cl_profiling_info     = cl_uint

  !
  ! OpenCL parameters
  !

  integer(cl_int), parameter :: CL_SUCCESS = 0
  integer(cl_int), parameter :: CL_ERROR   = 1

  ! cl_bool
  !
  integer(cl_bool), parameter :: CL_FALSE = 0
  integer(cl_bool), parameter :: CL_TRUE  = 1

  ! cl_device_type - bitfield
  !
  integer(cl_bitfield), parameter :: CL_DEVICE_TYPE_ALL = -1 ! c_val = 0xFFFFFFFF

  ! cl_command_queue_properties - bitfield
  !
  integer(cl_bitfield), parameter :: CL_QUEUE_PROFILING_ENABLE = 2  ! c_val = (1 << 1)

  ! cl_mem_flags - bitfield
  !
  integer(cl_bitfield), parameter :: CL_MEM_READ_WRITE     = 2**0   ! (1 << 0)
  integer(cl_bitfield), parameter :: CL_MEM_WRITE_ONLY     = 2**1   ! (1 << 1)
  integer(cl_bitfield), parameter :: CL_MEM_READ_ONLY      = 2**2   ! (1 << 2)
  integer(cl_bitfield), parameter :: CL_MEM_USE_HOST_PTR   = 2**3   ! (1 << 3)
  integer(cl_bitfield), parameter :: CL_MEM_ALLOC_HOST_PTR = 2**4   ! (1 << 4)
  integer(cl_bitfield), parameter :: CL_MEM_COPY_HOST_PTR  = 2**5   ! (1 << 5)

  ! cl_program_build_info
  !
  integer(cl_program_build_info), parameter :: CL_PROGRAM_BUILD_STATUS  = 4481  ! 0x1181
  integer(cl_program_build_info), parameter :: CL_PROGRAM_BUILD_OPTIONS = 4482  ! 0x1182
  integer(cl_program_build_info), parameter :: CL_PROGRAM_BUILD_LOG     = 4483  ! 0x1183

  ! cl_map_flags - bitfield
  !
  integer(cl_bitfield), parameter :: CL_MAP_READ  = 2**0            ! (1 << 0)
  integer(cl_bitfield), parameter :: CL_MAP_WRITE = 2**1            ! (1 << 1)

  ! cl_profiling_info
  !
  integer(cl_profiling_info), parameter :: CL_PROFILING_COMMAND_QUEUED = 4736  ! 0x1280
  integer(cl_profiling_info), parameter :: CL_PROFILING_COMMAND_SUBMIT = 4737  ! 0x1281
  integer(cl_profiling_info), parameter :: CL_PROFILING_COMMAND_START  = 4738  ! 0x1282
  integer(cl_profiling_info), parameter :: CL_PROFILING_COMMAND_END    = 4739  ! 0x1283

  type, BIND(C) :: cl_platform_id
    type(c_ptr) :: val
  end type cl_platform_id

  type(cl_platform_id), parameter :: platform_id_null = cl_platform_id(C_NULL_PTR)


!
! These should be moved to CLTypes?
!

   integer, parameter :: MAX_DEVICES = 2

   type :: CLDevice
      integer(c_int) :: device_id             ! device id (normally 0 for GPU, 1 for CPU)
      integer(c_int) :: num_devices           ! number of computing devices (cl_uint)
      type(c_ptr) :: device_ids(MAX_DEVICES)  ! compute device id (cl_device_id)
      type(c_ptr) :: context                  ! compute context (cl_context)
      type(c_ptr) :: commands                 ! compute command queue (cl_command_queue)
!   contains
!      procedure, pass(this) :: init
!      procedure, pass(this) :: createKernel
!      procedure, pass(this) :: createBuffer
   end type CLDevice

   type :: CLBuffer
      type(c_ptr) :: commands          ! compute command queue (cl_command_queue)
      type(c_ptr) :: event             ! event identifying the kernel execution instance
      logical :: mapped                ! true when buffer is mapped
      logical :: profiling             ! flag to enable profiling
      type(c_ptr) :: d_buf             ! handle to buffer on the device
      type(c_ptr) :: h_ptr             ! base address of host buffer (may be NULL)
      type(c_ptr) :: mapped_ptr        ! pointer to buffer on host (only valid when mapped)
      integer(c_size_t) :: size        ! size of buffer object
!   contains
!      procedure, pass(this) :: init
!      procedure, pass(this) :: clMemObject
!      procedure, pass(this) :: map
!      procedure, pass(this) :: unmap
   end type CLBuffer

   type :: CLKernel
      type(c_ptr) :: kernel            ! compute kernel
      type(c_ptr) :: commands          ! compute command queue (cl_command_queue)
      type(c_ptr) :: device            ! device we are using
      type(c_ptr) :: program           ! compute program
      type(c_ptr) :: event             ! event identifying the kernel execution instance
      logical     :: profiling         ! flag to enable profiling
      integer(c_int) :: elapsed        ! elapsed time in microseconds
      type(CPUTimer) :: timer          ! cpu timer for collecting event timing
!   contains
!      procedure, pass(this) :: init
!      procedure, pass(this) :: setKernelArgInt
!      procedure, pass(this) :: setKernelArgLoc
!      procedure, pass(this) :: setKernelArgMem
!      procedure, pass(this) :: setKernelArgReal
!      procedure, pass(this) :: run
   end type CLKernel

end module OpenCLTypes
