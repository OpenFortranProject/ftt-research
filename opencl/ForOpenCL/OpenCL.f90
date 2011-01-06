!
! This module creates bindings between Fortran and OpenCL.
!
! Currently the implementation relies on the following C types
! being pointers.  This true for at least Apples OpenCL implementation
! from cl.h
!
! typedef struct _cl_platform_id *    cl_platform_id;
! typedef struct _cl_device_id *      cl_device_id;
! typedef struct _cl_context *        cl_context;
! typedef struct _cl_command_queue *  cl_command_queue;
! typedef struct _cl_mem *            cl_mem;
! typedef struct _cl_program *        cl_program;
! typedef struct _cl_kernel *         cl_kernel;
! typedef struct _cl_event *          cl_event;
! typedef struct _cl_sampler *        cl_sampler;
!
!
! /* scalar types from Apple's cl_platform.h */
! typedef signed   __int8         cl_char;
! typedef unsigned __int8         cl_uchar;
! typedef signed   __int16        cl_short;
! typedef unsigned __int16        cl_ushort;
! typedef signed   __int32        cl_int;
! typedef unsigned __int32        cl_uint;
! typedef signed   __int64        cl_long;
! typedef unsigned __int64        cl_ulong;
! typedef unsigned __int16        cl_half;
! typedef float                   cl_float;
! typedef double                  cl_double;
!

module OpenCL
   use, intrinsic :: ISO_C_BINDING
   use :: OpenCLTypes
   use :: CLDevice_mod
   use :: CLKernel_mod
   use :: CLBuffer_mod
end module OpenCL
