module CLDevice_mod
   use, intrinsic :: ISO_C_BINDING
   use :: OpenCLTypes
   use :: OpenCLInterfaces

   integer, parameter :: MAX_DEVICES = 2

   type :: CLDevice
      integer(c_int) :: device_id             ! device id (normally 0 for GPU, 1 for CPU)
      integer(c_int) :: num_devices           ! number of computing devices (cl_uint)
      type(c_ptr) :: device_ids(MAX_DEVICES)  ! compute device id (cl_device_id)
      type(c_ptr) :: context                  ! compute context (cl_context)
      type(c_ptr) :: commands                 ! compute command queue (cl_command_queue)
   contains
      procedure, pass(this) :: init
      procedure, pass(this) :: createKernel
      procedure, pass(this) :: createBuffer
   end type CLDevice

contains

   function init(this, device_id) result(status)
      class(CLDevice) :: this
      integer(c_int) :: device_id
      integer(cl_bitfield) :: old_properties
      integer(cl_int) :: status

      this%device_id = device_id

      ! get number of devices available
      !
      status = clGetDeviceIDs(platform_id_null, CL_DEVICE_TYPE_ALL, &
                              MAX_DEVICES, this%device_ids, this%num_devices)

      if (status /= CL_SUCCESS) then
         print *, "CLDevice%initialize: Failed to find a device group"
         call stop_on_error(status)
      end if

      ! create a compute context
      !
      this%context = clCreateContext(C_NULL_PTR, 1, this%device_ids(1+this%device_id), &
                                     C_NULL_FUNPTR, C_NULL_PTR, status);

      if (status /= CL_SUCCESS) then
         print *, "CLDevice%initialize: Failed to create compute context for device", this%device_id
         call stop_on_error(status)
      end if

      ! create a command queue
      !
      this%commands = clCreateCommandQueue(this%context, this%device_ids(1+this%device_id), &
                                           0, status)
      if (status /= CL_SUCCESS) then
         print *, "CLDevice%initialize: Failed to create a command queue"
         call stop_on_error(status)
      end if

      ! turn on profiling for this command queue
      !
      status = clSetCommandQueueProperty(this%commands, CL_QUEUE_PROFILING_ENABLE, &
                                         CL_TRUE, old_properties)

      if (status /= CL_SUCCESS) then
         print *, "CLDevice%initialize: Failed to set command queue properties"
         call stop_on_error(status)
      end if

   end function init

   function createKernel(this, filename, name) result(kernel)
      use CLKernel_mod
      implicit none
      class(CLDevice) :: this
      character(*) :: filename
      character(*) :: name
      type(CLKernel) :: kernel
      integer(c_int) :: status
      
      status = kernel%init(this%context, this%commands, this%device_ids(1+this%device_id), filename, name)

   end function createKernel

   function createBuffer(this, size, host_ptr) result(cl_buf)
      use CLBuffer_mod
      implicit none
      class(CLDevice) :: this
      integer(c_size_t) :: size
      type(c_ptr) :: host_ptr
      type(CLBuffer) :: cl_buf
      integer(c_int) :: status
      
      status = cl_buf%init(this%context, this%commands, CL_MEM_USE_HOST_PTR, size, host_ptr)

   end function createBuffer

end module CLDevice_mod
