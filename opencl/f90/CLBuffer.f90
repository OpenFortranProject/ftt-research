module CLBuffer_mod
   use, intrinsic :: ISO_C_BINDING
   use :: OpenCLTypes
   use :: OpenCLInterfaces

   type :: CLBuffer
      type(c_ptr) :: commands          ! compute command queue (cl_command_queue)
      type(c_ptr) :: event             ! event identifying the kernel execution instance
      logical :: mapped                ! true when buffer is mapped
      logical :: profiling             ! flag to enable profiling
      type(c_ptr) :: d_buf             ! handle to buffer on the device
      type(c_ptr) :: h_ptr             ! pointer to buffer on host (only valid when mapped)
      integer(c_size_t) :: size        ! size of buffer object
   contains
      procedure, pass(this) :: init
      procedure, pass(this) :: clMemObject
      procedure, pass(this) :: map
      procedure, pass(this) :: unmap
   end type CLBuffer

contains

   function init(this, context, commands, flags, size, host_ptr) result(status)
      implicit none
      class(CLBuffer) :: this
      type(c_ptr) :: context, commands, host_ptr
      integer(cl_bitfield) :: flags
      integer(c_size_t) :: size
      integer(cl_int) :: status

      this%commands = commands
      this%size = size
      this%h_ptr = C_NULL_PTR

      this%d_buf = clCreateBuffer(context, flags, size, host_ptr, status)
      if (status /= CL_SUCCESS) then
         print *, "CLBuffer::CLBuffer: Failed to create buffer!"
         call stop_on_error(status)
      end if
   end function

   function clMemObject(this) result(mem_obj_rtn)
      implicit none
      class(CLBuffer) :: this
      type(c_ptr) :: mem_obj_rtn
      mem_obj_rtn = this%d_buf
   end function clMemObject

   function map(this, flags) result(h_ptr_ret)
      implicit none
      class(CLBuffer) :: this
      integer(cl_bitfield) :: flags
      type(c_ptr) :: h_ptr_ret
      integer(c_int) :: status

      this%h_ptr = clEnqueueMapBuffer(this%commands, this%d_buf, CL_TRUE, &
                                      flags, 0, this%size, 0, C_NULL_PTR, this%event, status)
      if (status /= CL_SUCCESS) then
         this%h_ptr = C_NULL_PTR
         print *, "CLBuffer::map: Failed to enqueue map buffer!"
         call stop_on_error(status)
      end if

      h_ptr_ret = this%h_ptr

   end function map

   function unmap(this) result(status)
      implicit none
      class(CLBuffer) :: this
      integer(c_int) :: status

      status = clEnqueueUnmapMemObject(this%commands, this%d_buf, this%h_ptr, 0, C_NULL_PTR, this%event)

      if (status /= CL_SUCCESS) then
         print *, "CLBuffer::unmap: Failed to enqueue unmap memory object!"
         call stop_on_error(status)
      end if

      this%h_ptr = C_NULL_PTR   ! buffer no longer mapped for host usage

   end function unmap

end module CLBuffer_mod

