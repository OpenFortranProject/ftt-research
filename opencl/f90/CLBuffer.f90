module CLBuffer_mod
   use, intrinsic :: ISO_C_BINDING
   use :: OpenCLTypes
   use :: OpenCLInterfaces

contains

   function init_buffer(this, context, commands, flags, size, host_ptr) result(status)
      implicit none
      !class(CLBuffer) :: this
      type(CLBuffer) :: this
      type(c_ptr) :: context, commands, host_ptr
      integer(cl_bitfield) :: flags
      integer(c_size_t) :: size
      integer(cl_int) :: status

      this%commands = commands
      this%size = size
      this%h_ptr = host_ptr
      this%mapped_ptr = C_NULL_PTR

      this%d_buf = clCreateBuffer(context, flags, size, host_ptr, status)
      if (status /= CL_SUCCESS) then
         print *, "CLBuffer::CLBuffer: Failed to create buffer!"
         call stop_on_error(status)
      end if
   end function init_buffer

   function clMemObject(this) result(mem_obj_rtn)
      implicit none
      !class(CLBuffer) :: this
      type(CLBuffer) :: this
      type(c_ptr) :: mem_obj_rtn
      mem_obj_rtn = this%d_buf
   end function clMemObject

   function readBuffer(this, dst, size) result(status)
      implicit none
      type(CLBuffer) :: this
      type(c_ptr) :: dst
      integer(c_size_t) :: size, offset
      integer(cl_int) :: status

      offset = 0
      status = clEnqueueReadBuffer(this%commands, this%d_buf, CL_TRUE, offset, size, &
                                   dst, 0, C_NULL_PTR, this%event)
      if (status /= CL_SUCCESS) then
         print *, "CLBuffer::readBuffer: Failed to enqueue read buffer!"
         call stop_on_error(status)
      end if
   end function readBuffer

   function copyBuffer(src, dst, size) result(status)
      implicit none
      type(CLBuffer) :: src, dst
      integer(c_size_t) :: size, offset
      integer(cl_int) :: status

      offset = 0
      ! assumes src and dst have same command queue
      status = clEnqueueCopyBuffer(src%commands, src%d_buf, dst%d_buf, offset, offset, size, &
                                   0, C_NULL_PTR, src%event)
      if (status /= CL_SUCCESS) then
         print *, "CLBuffer::copyBuffer: Failed to enqueue copy buffer!"
         call stop_on_error(status)
      end if

   end function copyBuffer

   function map(this, flags) result(mapped_ptr_ret)
      implicit none
      !class(CLBuffer) :: this
      type(CLBuffer) :: this
      integer(cl_bitfield) :: flags
      type(c_ptr) :: mapped_ptr_ret
      integer(c_int) :: status

      integer(c_size_t) :: offset = 0

      this%mapped_ptr = clEnqueueMapBuffer(this%commands, this%d_buf, CL_TRUE, flags, &
                                           offset, this%size, 0, C_NULL_PTR, this%event, status)
      if (status /= CL_SUCCESS) then
         this%mapped_ptr = C_NULL_PTR
         print *, "CLBuffer::map: Failed to enqueue map buffer!"
         call stop_on_error(status)
      end if

      mapped_ptr_ret = this%mapped_ptr

   end function map

   function unmap(this) result(status)
      implicit none
      !class(CLBuffer) :: this
      type(CLBuffer) :: this
      integer(c_int) :: status

      status = clEnqueueUnmapMemObject(this%commands, this%d_buf, this%mapped_ptr, &
                                       0, C_NULL_PTR, this%event)

      if (status /= CL_SUCCESS) then
         print *, "CLBuffer::unmap: Failed to enqueue unmap memory object!"
         call stop_on_error(status)
      end if

      this%mapped_ptr = C_NULL_PTR   ! buffer no longer mapped for host usage

   end function unmap

end module CLBuffer_mod
