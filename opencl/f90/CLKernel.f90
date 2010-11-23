module CLKernel_mod
   use, intrinsic :: ISO_C_BINDING
   use :: OpenCLTypes
   use :: OpenCLInterfaces

!   interface setKernelArg
!      module procedure setKernelArgInt, setKernelArgMem, setKernelArgLoc, &
!                       setKernelArgReal
!   end interface

   interface init
      module procedure init_kernel
   end interface init

contains

   function init_kernel(this, context, commands, device, filename, name) result(status)
      use Timer_mod
      implicit none
      !class(CLKernel) :: this
      type(CLKernel) :: this
      type(c_ptr) :: context, commands, device
      character(*) :: filename, name
      integer(cl_int) :: status, status_save

      integer(c_size_t) :: lengths(1)
      type(c_ptr) :: source

      integer(c_size_t) :: err_len
      integer(c_size_t), parameter :: err_buf_len = 2048
      character :: err_buf(err_buf_len)

      this%device = device
      this%commands = commands
      this%profiling = .false.
      this%elapsed = 0

      call init(this%timer)

      status = CL_SUCCESS

      ! Create the compute program from the source buffer
      !
      source = load_program_source(filename, lengths)

      this%program = clCreateProgramWithSource(context, 1, source, lengths, status)
      call c_free(source)
      if (status /= CL_SUCCESS) then
         print *, "CLKernel::init: Failed to create compute program!\n"
         call stop_on_error(status)
      end if

      ! Build the program executable
      !
      status = clBuildProgram(this%program, 0, C_NULL_PTR, &
                              "-cl-fast-relaxed-math" // C_NULL_CHAR, C_NULL_FUNPTR, C_NULL_PTR)
      if (status /= CL_SUCCESS) then
         status_save = status

         print *, "CLKernel::init: Failed to build program executable!"
         status = clGetProgramBuildInfo(this%program, this%device, &
                                        CL_PROGRAM_BUILD_LOG, err_buf_len, err_buf, err_len)
         if (status /= CL_SUCCESS) then
            print *, "CLKernel::init: error buf length may be too small, is", 2048, " should be", err_len
            print *
            print *, err_buf
            call stop_on_error(status)
         end if

         call stop_on_error(status_save)
      end if

      ! Create the compute kernel in the program we wish to run
      !
      this%kernel = clCreateKernel(this%program, name, status)
      if (status /= CL_SUCCESS) then
         print *, "CLKernel::init: Failed to create compute kernel!"
         call stop_on_error(status)
      end if
   end function init_kernel

   function setKernelArgInt(this, argid, int_var) result(status)
      implicit none
      !class(CLKernel) :: this
      type(CLKernel) :: this
      integer, intent(in) :: argid
      integer(c_int), target :: int_var
      integer(cl_int) :: status
      integer(c_size_t) :: arg_size

   ! TODO - FIXME - sizeof_int
      arg_size = 4

      status = clSetKernelArg(this%kernel, argid, arg_size, c_loc(int_var))
      if (status /= CL_SUCCESS) then
         print *, "CLDevice::setKernelArgInt: Failed to set kernel argument!"
         call stop_on_error(status)
      end if
   end function setKernelArgInt

   function setKernelArgLoc(this, argid, size) result(status)
      implicit none
      !class(CLKernel) :: this
      type(CLKernel) :: this
      integer, intent(in) :: argid
      integer(c_size_t) :: size
      integer(cl_int) :: status

      status = clSetKernelArg(this%kernel, argid, size, C_NULL_PTR)
      if (status /= CL_SUCCESS) then
         print *, "CLDevice::setKernelArgLoc: Failed to set kernel argument!"
         call stop_on_error(status)
      end if
   end function setKernelArgLoc

   function setKernelArgMem(this, argid, cl_mem_obj) result(status)
      implicit none
      !class(CLKernel) :: this
      type(CLKernel) :: this
      integer, intent(in) :: argid
      type(c_ptr), target :: cl_mem_obj
      integer(cl_int) :: status

      status = clSetKernelArg(this%kernel, argid, c_sizeof_cl_mem(), c_loc(cl_mem_obj))
      if (status /= CL_SUCCESS) then
         print *, "CLDevice::setKernelArgMem: Failed to set kernel argument!"
         call stop_on_error(status)
      end if
   end function setKernelArgMem

   function setKernelArgReal(this, argid, real_var) result(status)
      implicit none
      !class(CLKernel) :: this
      type(CLKernel) :: this
      integer, intent(in) :: argid
      real(c_float), target :: real_var
      integer(cl_int) :: status
      integer(c_size_t) :: arg_size

   ! TODO - FIXME - sizeof_int
      arg_size = 4

      status = clSetKernelArg(this%kernel, argid, arg_size, c_loc(real_var))
      if (status /= CL_SUCCESS) then
         print *, "CLDevice::setKernelArgReal: Failed to set kernel argument!"
         call stop_on_error(status)
      end if
   end function setKernelArgReal

   function run(this, gWorkSizeX, gWorkSizeY, lWorkSizeX, lWorkSizeY) result(status)
      implicit none
      !class(CLKernel) :: this
      type(CLKernel) :: this
      integer(c_size_t) :: gWorkSizeX, gWorkSizeY, lWorkSizeX, lWorkSizeY
      integer(cl_int) :: status

      integer(c_size_t), dimension(2) :: global_work_offset, local_work_size, global_work_size
      integer(cl_ulong), target :: prof_start, prof_end
      integer(c_size_t) :: param_size

      global_work_offset = 0
      global_work_size = [gWorkSizeX, gWorkSizeY]
      local_work_size  = [lWorkSizeX, lWorkSizeY]

      ! execute the kernel
      !
      call start(this%timer)
      status = clEnqueueNDRangeKernel(this%commands, this%kernel, 2, global_work_offset, &
                                      global_work_size, local_work_size, 0, C_NULL_PTR, C_NULL_PTR)
      call stop(this%timer)

      if (status /= CL_SUCCESS) then
         print *, "CLDevice::run(): Failed to execute kernel!"
         ! print *, "CLDevice::run(): max_local_work_size==%ld\n", max_local_size)
         call stop_on_error(status)
      end if

      ! wait for the command commands to get serviced before reading back results
      !
!      call start(this%timer)
!      status = clFinish(this%commands)
!      call stop(this%timer)

      if (this%profiling) then
         status = clGetEventProfilingInfo(this%event, CL_PROFILING_COMMAND_START, &
                                          c_sizeof_cl_ulong(), c_loc(prof_start), param_size)
         status = clGetEventProfilingInfo(this%event, CL_PROFILING_COMMAND_END,   &
                                          c_sizeof_cl_ulong(), c_loc(prof_end), param_size)
         if (status == 0) then
            this%elapsed = (prof_end - prof_start) / 1000   ! microseconds
            ! print *, "kernel::run: elapsed=", this%elapsed
         endif

      end if

   end function run

end module CLKernel_mod
