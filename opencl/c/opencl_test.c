#include <OpenCL/opencl.h>
#include <stdio.h>
#include <stdlib.h>

cl_int
clGetDeviceIDs_test(cl_platform_id platform,
                    cl_device_type device_type,
                    cl_uint num_entries,
                    cl_device_id * devices,
                    cl_uint * num_devices)
{
   int status;
   printf("clGetDeviceIDs_test: platform==%p num_entries==%d\n", platform, num_entries);
   status = clGetDeviceIDs(platform, device_type, num_entries, devices, num_devices);
   printf("clGetDeviceIDs_test: status==%d num_devices==%d\n", status, *num_devices);
   return status;
}

cl_context
clCreateContext_test(const cl_context_properties * properties,
                     cl_uint num_devices,
                     const cl_device_id * devices,
                     void (*pfn_notify)(const char *, const void *, size_t, void *),
                     void * user_data,
                     cl_int * status)
{
   cl_context context;
   context = clCreateContext(properties, num_devices, devices, NULL, user_data, status);
   printf("clCreateContext_test: context==%p\n", context);
   return context;
}

cl_command_queue
clCreateCommandQueue_test(cl_context                   context,
                          cl_device_id                 device,
                          cl_command_queue_properties  properties,
                          cl_int *                     status)
{
   printf("clCreateCommandQueue_test: context==%p properties==%ld\n", context, (long) properties);
   return clCreateCommandQueue(context, device, properties, status);
}


cl_int
clSetCommandQueueProperty_test(cl_command_queue               command_queue,
                               cl_command_queue_properties    properties, 
                               cl_bool                        enable,
                               cl_command_queue_properties *  old_properties)
{
   cl_int status;
   printf("clSetCommandQueueProperty_test: properties==%ld enable==%d\n", (long) properties, enable);
   status = clSetCommandQueueProperty(command_queue, properties, enable, old_properties);
   return status;
}


cl_program
clCreateProgramWithSource_test(cl_context        context,
                               cl_uint           count,
                               const char **     strings,
                               const size_t *    lengths,
                               cl_int *          errcode_ret)
{
   cl_program program;
   printf("clCreateProgramWithSource_test: context==%p count==%d length==%ld source==\n%s\n",
          context, count, lengths[0], strings[0]);
   program = clCreateProgramWithSource(context, count, strings, lengths, errcode_ret);
   printf("clCreateProgramWithSource_test: program==%p\n", program);
   return program;
}


cl_int
clBuildProgram_test(cl_program           program,
                    cl_uint              num_devices,
                    const cl_device_id * device_list,
                    const char *         options, 
                    void (*pfn_notify)(cl_program /* program */, void * /* user_data */),
                    void *               user_data)
{
   printf("clBuildProgram_test: program==%p num_devices==%d\n", program, num_devices);
   //   return clBuildProgram(program, num_devices, device_list, options, pfn_notify, user_data);
   return clBuildProgram(program, num_devices, device_list, options, pfn_notify, user_data);
}


cl_kernel
clCreateKernel_test(cl_program   program,
                    const char * kernel_name,
                    cl_int *     errcode_ret)
{
   printf("clCreateKernel_test: program==%p kernel_name==%s\n", program, kernel_name);
   return clCreateKernel(program, kernel_name, errcode_ret);
}

cl_int
clSetKernelArg_test(cl_kernel    kernel,
                    cl_uint      arg_index,
                    size_t       arg_size,
                    const void * arg_value)
{
   printf("clSetKernelArg_test: kernel==%p arg_index==%d arg_size=%ld\n", kernel, arg_index, arg_size);
   return clSetKernelArg(kernel, arg_index, arg_size, arg_value);
}

cl_mem
clCreateBuffer_test(cl_context   context,
                    cl_mem_flags flags,
                    size_t       size,
                    void *       host_ptr,
                    cl_int *     errcode_ret)
{
   cl_mem buf_rtn;
   buf_rtn = clCreateBuffer(context, flags, size, host_ptr, errcode_ret);
   printf("clCreateBuffer_test: context==%p flags==%ld size==%ld host_ptr==%p buf_rtn==%p\n",
          context, (long) flags, size, host_ptr, buf_rtn);
   return buf_rtn;
}


void *
clEnqueueMapBuffer_test(cl_command_queue  command_queue,
                        cl_mem            buffer,
                        cl_bool           blocking_map,
                        cl_map_flags      map_flags,
                        size_t            offset,
                        size_t            cb,
                        cl_uint           num_events_in_wait_list,
                        const cl_event *  event_wait_list,
                        cl_event *        event,
                        cl_int *          errcode_ret)
{
   void * host_ptr_ret;
   /* WARNING, event not specified correctly for non-blocking map */
   host_ptr_ret = clEnqueueMapBuffer(command_queue, buffer, blocking_map, map_flags, offset, cb,
                              num_events_in_wait_list, event_wait_list, NULL /*event*/, errcode_ret);
   printf("clEnqueueMapBuffer_test: buf==%p flags==%ld offset==%ld event==%p host_ptr==%p\n",
          buffer, (long) map_flags, offset, event, host_ptr_ret);
   return host_ptr_ret;
}

cl_int
clEnqueueUnmapMemObject_test(cl_command_queue command_queue,
                             cl_mem           memobj,
                             void *           mapped_ptr,
                             cl_uint          num_events_in_wait_list,
                             const cl_event * event_wait_list,
                             cl_event *       event)
{
   printf("clEnqueueUnmapMemObject_test: memobj==%p ptr==%p event==%p\n", memobj, mapped_ptr, event);
   return clEnqueueUnmapMemObject(command_queue, memobj, mapped_ptr,
                                  num_events_in_wait_list, event_wait_list, event);

}

cl_int
clEnqueueNDRangeKernel_test(cl_command_queue command_queue,
                            cl_kernel        kernel,
                            cl_uint          work_dim,
                            const size_t *   global_work_offset,
                            const size_t *   global_work_size,
                            const size_t *   local_work_size,
                            cl_uint          num_events_in_wait_list,
                            const cl_event * event_wait_list,
                            cl_event *       event)
{
   printf("clEnqueueNDRangeKernel_test: wk_dim==%d g_work_size==%ld,%ld l_work_size==%ld,%ld\n",
          work_dim, global_work_size[0], global_work_size[1], local_work_size[0], local_work_size[1]);
   return clEnqueueNDRangeKernel(command_queue,
                                 kernel,
                                 work_dim,
                                 global_work_offset,
                                 global_work_size,
                                 local_work_size,
                                 num_events_in_wait_list,
                                 event_wait_list,
                                 event);
}
