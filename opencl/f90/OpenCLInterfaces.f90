module OpenCLInterfaces

interface

   ! Platform API
   !

   ! cl_int
   ! clGetPlatformIDs(cl_uint          /* num_entries */,
   !                  cl_platform_id * /* platforms */,
   !                  cl_uint *        /* num_platforms */);
   !


   ! Device APIs
   !

   ! cl_int
   ! clGetDeviceIDs(cl_platform_id   /* platform */,
   !                cl_device_type   /* device_type */, 
   !                cl_uint          /* num_entries */, 
   !                cl_device_id *   /* devices */, 
   !                cl_uint *        /* num_devices */);
   !
   function clGetDeviceIDs(platform, device_type, num_entries, devices, num_devices) &
            result(status) bind(C, name="clGetDeviceIDs")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(cl_platform_id), value :: platform
      integer(cl_bitfield), value :: device_type
      integer(cl_uint), value :: num_entries
      type(c_ptr) :: devices(*)
      integer(cl_uint), intent(out) :: num_devices
      integer(cl_int) :: status
   end function clGetDeviceIDs

   ! Context APIs
   !

   ! cl_context
   ! clCreateContext(const cl_context_properties * /* properties */,
   !                 cl_uint                       /* num_devices */,
   !                 const cl_device_id *          /* devices */,
   !                 void (*pfn_notify)(const char *, const void *, size_t, void *) /* pfn_notify */,
   !                 void *                        /* user_data */,
   !                 cl_int *                      /* errcode_ret */);
   function clCreateContext(properties, num_devices, devices, pfn_notify, user_data, errcode_ret) &
            result(context_ret) bind(C, name="clCreateContext")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: properties
      integer(cl_uint), value :: num_devices
      type(c_ptr) :: devices(*)
      type(c_funptr), value :: pfn_notify
      type(c_ptr), value :: user_data
      integer(cl_int), intent(out) :: errcode_ret
      type(c_ptr) :: context_ret
   end function clCreateContext

   ! Command Queue APIs
   !

   ! cl_command_queue
   ! clCreateCommandQueue(cl_context                    /* context */, 
   !                      cl_device_id                  /* device */, 
   !                      cl_command_queue_properties   /* properties */,
   !                      cl_int *                      /* errcode_ret */);
   function clCreateCommandQueue(context, device, properties, errcode_ret) &
            result(command_queue_ret) bind(C, name="clCreateCommandQueue")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: context
      type(c_ptr), value :: device
      integer(cl_bitfield), value :: properties
      integer(cl_int), intent(out) :: errcode_ret
      type(c_ptr) :: command_queue_ret
   end function clCreateCommandQueue


   ! Command Queue APIs
   !

   ! cl_int
   ! clSetCommandQueueProperty(cl_command_queue              /* command_queue */,
   !                           cl_command_queue_properties   /* properties */, 
   !                           cl_bool                       /* enable */,
   !                           cl_command_queue_properties * /* old_properties */);

   function clSetCommandQueueProperty(command_queue, properties, enable, old_properties) &
            result(status) bind(C, name="clSetCommandQueueProperty")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: command_queue
      integer(cl_bitfield), value :: properties
      integer(cl_bool), value :: enable
      integer(cl_bitfield) :: old_properties
      integer(cl_int) :: status
   end function clSetCommandQueueProperty


   ! Program Object APIs
   !

   ! cl_program
   ! clCreateProgramWithSource(cl_context        /* context */,
   !                           cl_uint           /* count */,
   !                           const char **     /* strings */,
   !                           const size_t *    /* lengths */,
   !                           cl_int *          /* errcode_ret */);
   function clCreateProgramWithSource(context, count, strings, lengths, errcode_ret) &
            result(program_ret) bind(C, name="clCreateProgramWithSource")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: context
      integer(cl_uint), value :: count
      type(c_ptr) :: strings
      integer(c_size_t), dimension(*) :: lengths
      integer(cl_int) :: errcode_ret
      type(c_ptr) :: program_ret
   end function clCreateProgramWithSource

   ! cl_int
   ! clBuildProgram(cl_program           /* program */,
   !                cl_uint              /* num_devices */,
   !                const cl_device_id * /* device_list */,
   !                const char *         /* options */, 
   !                void (*pfn_notify)(cl_program /* program */, void * /* user_data */),
   !                void *               /* user_data */);
   function clBuildProgram(program, num_devices, device_list, options, pfn_notify, user_data) &
            result(status) bind(C, name="clBuildProgram")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: program
      integer(cl_uint), value :: num_devices
      type(c_ptr), value :: device_list
      character(kind=c_char), dimension(*) :: options
      type(c_funptr), value :: pfn_notify
      type(c_ptr), value :: user_data
      integer(cl_int) :: status
   end function clBuildProgram
      
   ! cl_int
   ! clGetProgramBuildInfo(cl_program            /* program */,
   !                       cl_device_id          /* device */,
   !                       cl_program_build_info /* param_name */,
   !                       size_t                /* param_value_size */,
   !                       void *                /* param_value */,
   !                       size_t *              /* param_value_size_ret */);
   function clGetProgramBuildInfo(program, device, param_name, &
                                  param_value_size, param_value, param_value_size_ret) &
            result(status) bind(C, name="clGetProgramBuildInfo_test")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: program
      type(c_ptr), value :: device
      integer(cl_program_build_info), value :: param_name
      integer(c_size_t), value :: param_value_size
      character(kind=c_char), dimension(*) :: param_value
      integer(c_size_t), intent(out) :: param_value_size_ret
      integer(cl_int) :: status
   end function clGetProgramBuildInfo


   ! Kernel Object APIs
   !

   ! cl_kernel
   ! clCreateKernel(cl_program      /* program */,
   !                const char *    /* kernel_name */,
   !                cl_int *        /* errcode_ret */);
   function clCreateKernel(program, kernel_name, errcode_ret) &
            result(kernel_ret) bind(C, name="clCreateKernel")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: program
      character(kind=c_char), dimension(*) :: kernel_name
      integer(cl_int) :: errcode_ret
      type(c_ptr) :: kernel_ret
   end function clCreateKernel      

   ! cl_int
   ! clSetKernelArg(cl_kernel    /* kernel */,
   !                cl_uint      /* arg_index */,
   !                size_t       /* arg_size */,
   !                const void * /* arg_value */);
   function clSetKernelArg(kernel, arg_index, arg_size, arg_value) &
            result(status) bind(C, name="clSetKernelArg")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: kernel
      integer(cl_uint), value :: arg_index
      integer(c_size_t), value :: arg_size
      type(c_ptr), value :: arg_value
      integer(cl_int) :: status
   end function clSetKernelArg


   ! Memory Object APIs
   !

   ! cl_mem
   ! clCreateBuffer(cl_context   /* context */,
   !                cl_mem_flags /* flags */,
   !                size_t       /* size */,
   !                void *       /* host_ptr */,
   !                cl_int *     /* errcode_ret */);
   function clCreateBuffer(context, flags, size, host_ptr, errcode_ret) &
            result(buffer_ret) bind(C, name="clCreateBuffer")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: context
      integer(cl_bitfield), value :: flags
      integer(c_size_t), value :: size
      type(c_ptr), value :: host_ptr
      integer(cl_int) :: errcode_ret
      type(c_ptr) :: buffer_ret
   end function clCreateBuffer


   ! Profiling APIs
   !

   ! cl_int
   ! clGetEventProfilingInfo(cl_event            /* event */,
   !                         cl_profiling_info   /* param_name */,
   !                         size_t              /* param_value_size */,
   !                         void *              /* param_value */,
   !                         size_t *            /* param_value_size_ret */);
   function clGetEventProfilingInfo(event, param_name, param_value_size, &
                                    param_value, param_value_size_ret)   &
            result(status) bind(C,name="clGetEventProfilingInfo")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: event
      integer(cl_profiling_info), value :: param_name
      integer(c_size_t), value :: param_value_size
      type(c_ptr), value :: param_value
      integer(c_size_t) :: param_value_size_ret
      integer(cl_int) :: status
   end function clGetEventProfilingInfo


   ! Flush and Finish APIs
   !

   ! cl_int
   ! clFlush(cl_command_queue /* command_queue */);
   function clFlush(command_queue) result(status) bind(C, name="clFlush")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: command_queue
      integer(cl_int) :: status
   end function clFlush

   ! cl_int
   ! clFinish(cl_command_queue /* command_queue */);
   function clFinish(command_queue) result(status) bind(C, name="clFinish")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: command_queue
      integer(cl_int) :: status
   end function clFinish


   ! Enqueued Commands APIs
   !

   ! cl_int
   ! clEnqueueReadBuffer(cl_command_queue    /* command_queue */,
   !                     cl_mem              /* buffer */,
   !                     cl_bool             /* blocking_read */,
   !                     size_t              /* offset */,
   !                     size_t              /* cb */, 
   !                     void *              /* ptr */,
   !                     cl_uint             /* num_events_in_wait_list */,
   !                     const cl_event *    /* event_wait_list */,
   !                    cl_event *          /* event */);
   function clEnqueueReadBuffer(command_queue, buffer, blocking_read, offset, cb, &
                                ptr, num_events_in_wait_list, event_wait_list, event) &
            result(status) bind(C,name="clEnqueueReadBuffer")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: command_queue
      type(c_ptr), value :: buffer
      integer(cl_bool), value :: blocking_read
      integer(c_size_t), value :: offset, cb
      type(c_ptr), value :: ptr
      integer(cl_uint), value :: num_events_in_wait_list
      type(c_ptr), value :: event_wait_list
      type(c_ptr) :: event
      integer(cl_int) :: status
   end function clEnqueueReadBuffer
                            
   ! cl_int
   ! clEnqueueWriteBuffer(cl_command_queue   /* command_queue */, 
   !                      cl_mem             /* buffer */, 
   !                      cl_bool            /* blocking_write */, 
   !                      size_t             /* offset */, 
   !                      size_t             /* cb */, 
   !                      const void *       /* ptr */, 
   !                      cl_uint            /* num_events_in_wait_list */, 
   !                      const cl_event *   /* event_wait_list */, 
   !                      cl_event *         /* event */);
   function clEnqueueWriteBuffer(command_queue, buffer, blocking_write, offset, cb, &
                                 ptr, num_events_in_wait_list, event_wait_list, event) &
            result(status) bind(C,name="clEnqueueWriteBuffer")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: command_queue
      type(c_ptr), value :: buffer
      integer(cl_bool), value :: blocking_write
      integer(c_size_t), value :: offset, cb
      type(c_ptr), value :: ptr
      integer(cl_uint), value :: num_events_in_wait_list
      type(c_ptr), value :: event_wait_list
      type(c_ptr) :: event
      integer(cl_int) :: status
   end function clEnqueueWriteBuffer
                            
   ! cl_int
   ! clEnqueueCopyBuffer(cl_command_queue    /* command_queue */, 
   !                     cl_mem              /* src_buffer */,
   !                     cl_mem              /* dst_buffer */, 
   !                     size_t              /* src_offset */,
   !                     size_t              /* dst_offset */,
   !                     size_t              /* cb */, 
   !                     cl_uint             /* num_events_in_wait_list */,
   !                     const cl_event *    /* event_wait_list */,
   !                     cl_event *          /* event */);
   function clEnqueueCopyBuffer(command_queue, src_buffer, dst_buffer, src_offset, dst_offset, &
                                cb, num_events_in_wait_list, event_wait_list, event) &
            result(status) bind(C,name="clEnqueueCopyBuffer")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: command_queue
      type(c_ptr), value :: src_buffer, dst_buffer
      integer(c_size_t), value :: src_offset, dst_offset, cb
      integer(cl_uint), value :: num_events_in_wait_list
      type(c_ptr), value :: event_wait_list
      type(c_ptr) :: event
      integer(cl_int) :: status
   end function clEnqueueCopyBuffer

   ! void *
   ! clEnqueueMapBuffer(cl_command_queue /* command_queue */,
   !                    cl_mem           /* buffer */,
   !                    cl_bool          /* blocking_map */, 
   !                    cl_map_flags     /* map_flags */,
   !                    size_t           /* offset */,
   !                    size_t           /* cb */,
   !                    cl_uint          /* num_events_in_wait_list */,
   !                    const cl_event * /* event_wait_list */,
   !                    cl_event *       /* event */,
   !                    cl_int *         /* errcode_ret */);
   function clEnqueueMapBuffer(command_queue, buffer, blocking_map, map_flags, offset, &
                               cb, num_events_in_wait_list, event_wait_list, event, errcode_ret) &
            result(host_ptr_ret) bind(C, name="clEnqueueMapBuffer_test")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: command_queue
      type(c_ptr), value :: buffer
      integer(cl_bool), value :: blocking_map
      integer(cl_bitfield), value :: map_flags
      integer(c_size_t), value :: offset
      integer(c_size_t), value :: cb
      integer(cl_uint), value :: num_events_in_wait_list
      type(c_ptr), value :: event_wait_list
      type(c_ptr) :: event
      integer(cl_int) :: errcode_ret
      type(c_ptr) :: host_ptr_ret
   end function clEnqueueMapBuffer

   ! cl_int
   ! clEnqueueUnmapMemObject(cl_command_queue /* command_queue */,
   !                         cl_mem           /* memobj */,
   !                         void *           /* mapped_ptr */,
   !                         cl_uint          /* num_events_in_wait_list */,
   !                         const cl_event * /* event_wait_list */,
   !                         cl_event *       /* event */);
   function clEnqueueUnmapMemObject(command_queue, memobj, mapped_ptr, &
                                    num_events_in_wait_list, event_wait_list, event) &
            result(status) bind(C, name="clEnqueueUnmapMemObject")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: command_queue
      type(c_ptr), value :: memobj
      type(c_ptr), value :: mapped_ptr
      integer(cl_uint), value :: num_events_in_wait_list
      type(c_ptr), value :: event_wait_list
      type(c_ptr) :: event
      integer(cl_int) :: status
   end function clEnqueueUnmapMemObject

   ! cl_int
   ! clEnqueueNDRangeKernel(cl_command_queue /* command_queue */,
   !                        cl_kernel        /* kernel */,
   !                        cl_uint          /* work_dim */,
   !                        const size_t *   /* global_work_offset */,
   !                        const size_t *   /* global_work_size */,
   !                        const size_t *   /* local_work_size */,
   !                        cl_uint          /* num_events_in_wait_list */,
   !                        const cl_event * /* event_wait_list */,
   !                        cl_event *       /* event */);
   function clEnqueueNDRangeKernel(command_queue, kernel, work_dim, global_work_offset, &
                                   global_work_size, local_work_size, num_events_in_wait_list, &
                                   event_wait_list, event) &
            result(status) bind(C, name="clEnqueueNDRangeKernel")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      type(c_ptr), value :: command_queue
      type(c_ptr), value :: kernel
      integer(cl_uint), value :: work_dim
      integer(c_size_t), dimension(*) :: global_work_offset, global_work_size, local_work_size
      integer(cl_uint), value :: num_events_in_wait_list
      type(c_ptr), value :: event_wait_list
      type(c_ptr) :: event
      integer(cl_int) :: status
   end function clEnqueueNDRangeKernel


   !
   ! C utility functions
   !

   function load_program_source(filename, count) &
            result(source) bind(C, name="load_program_source")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      character(kind=c_char), dimension(*) :: filename
      integer(c_size_t), dimension(*) :: count
      type(c_ptr) :: source
   end function

   function c_sizeof_cl_mem() result(size) bind(C, name="c_sizeof_cl_mem")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      integer(c_size_t) :: size
   end function

   function c_sizeof_cl_ulong() result(size) bind(C, name="c_sizeof_cl_ulong")
      use, intrinsic :: ISO_C_BINDING
      use :: OpenCLTypes
      implicit none
      integer(c_size_t) :: size
   end function

   subroutine c_free(buf) bind(C, name="c_free")
      use, intrinsic :: ISO_C_BINDING
      implicit none
      type(c_ptr), value :: buf
   end subroutine

   subroutine stop_on_error(code) bind(C, name="stop_on_error")
      use, intrinsic :: ISO_C_BINDING
      implicit none
      integer(c_int), value :: code
   end subroutine

   subroutine print_addr(addr) bind(C, name="print_addr")
     use, intrinsic :: ISO_C_BINDING
     implicit none
     type(c_ptr), value :: addr
   end subroutine

end interface

end module OpenCLInterfaces
