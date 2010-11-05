module OpenCLTypes
  use, intrinsic :: ISO_C_BINDING

  integer, parameter :: cl_int      = C_INT
  integer, parameter :: cl_uint     = C_INT
  integer, parameter :: cl_ulong    = C_LONG
  integer, parameter :: cl_bool     = cl_int
  integer, parameter :: cl_bitfield = cl_ulong

  integer, parameter :: cl_program_build_info = cl_uint

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

  type :: cl_platform_id
    type(c_ptr) :: val
  end type cl_platform_id

  type(cl_platform_id), parameter :: platform_id_null = cl_platform_id(C_NULL_PTR)

end module OpenCLTypes
