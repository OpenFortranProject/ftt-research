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

module ForOpenCL
   use, intrinsic :: ISO_C_BINDING
   use :: OpenCLTypes
   use :: CLDevice_mod
   use :: CLKernel_mod
   use :: CLBuffer_mod

contains

function get_subimage(device_id, cl_device_)
   use CLDevice_mod, only : init_device, CLDevice
   implicit none
   integer, intent(in) :: device_id
   type(CLDevice), intent(out) :: cl_device_
   integer :: get_subimage
   integer(cl_int) :: status

   status = init_device(cl_device_, int(device_id,c_int))
   get_subimage = device_id

end function get_subimage

function focl_global_size(rank, lws, prev_gws, new_gws) result(rtn_gws)
  integer,           intent(in) :: rank
  integer(c_size_t), intent(in) :: lws(*), prev_gws(*), new_gws(*)
  integer(c_size_t)             :: rtn_gws(3)
  integer                       :: i, dim

  rtn_gws = [1,1,1]

  dim = rank
  if (dim > 3) dim = 3

  !! Need to consider local work group size and nice even numbers
  !
  do i = 1, dim
     if (prev_gws(i) > 1 .AND. new_gws(i) > 1) then
        rtn_gws(i) = min(prev_gws(i), new_gws(i))
     else
        rtn_gws(i) = max(prev_gws(i), new_gws(i))
     end if
  end do

end function focl_global_size

end module ForOpenCL
