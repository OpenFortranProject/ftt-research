module CLDevice_mod
   use, intrinsic :: ISO_C_BINDING
   use :: OpenCLTypes
   use :: OpenCLInterfaces
   use :: CLKernel_mod
   use :: CLBuffer_mod

contains

   function init(this, device_id) result(status)
      !class(CLDevice) :: this
      type(CLDevice) :: this
      integer(c_int) :: device_id
      integer(cl_bitfield) :: properties, old_properties
      integer(cl_int) :: status

      integer(cl_uint) :: num_platforms
      type(c_ptr) :: platforms(2)

      this%device_id = device_id

      ! get and use the first platform available
      !
      status = clGetPlatformIDs(2, platforms, num_platforms)
      if (num_platforms > 1) then
         print *, "number of platforms =", num_platforms
      end if

      ! get number of devices available
      !
      status = clGetDeviceIDs(platforms(1), CL_DEVICE_TYPE_ALL, &
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
      properties = 0  ! NULL
      this%commands = clCreateCommandQueue(this%context, this%device_ids(1+this%device_id), &
                                           properties, status)
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
      !class(CLDevice) :: this
      type(CLDevice) :: this
      character(*) :: filename
      character(*) :: name
      type(CLKernel) :: kernel
      integer(c_int) :: status
      
      status = init_kernel(kernel, this%context, this%commands, &
                           this%device_ids(1+this%device_id), filename, name)

   end function createKernel

   function createBuffer(this, flags, size, host_ptr) result(cl_buf)
      use CLBuffer_mod
      implicit none
      !class(CLDevice) :: this
      type(CLDevice) :: this
      integer(cl_bitfield) :: flags
      integer(c_size_t) :: size
      type(c_ptr) :: host_ptr
      type(CLBuffer) :: cl_buf
      integer(c_int) :: status
      
      status = init_buffer(cl_buf, this%context, this%commands, flags, size, host_ptr)

   end function createBuffer

   function createBufferMapped(this, size, host_ptr) result(cl_buf)
      use CLBuffer_mod
      implicit none
      !class(CLDevice) :: this
      type(CLDevice) :: this
      integer(c_size_t) :: size
      type(c_ptr) :: host_ptr
      type(CLBuffer) :: cl_buf
      integer(c_int) :: status
      
      status = init_buffer(cl_buf, this%context, this%commands, &
                           CL_MEM_USE_HOST_PTR, size, host_ptr)

   end function createBufferMapped

end module CLDevice_mod
