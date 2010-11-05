module CLKernel_mod
   use, intrinsic :: ISO_C_BINDING
   use :: OpenCLTypes
   use :: OpenCLInterfaces

   type :: CLKernel
      type(c_ptr) :: kernel            ! compute kernel
      type(c_ptr) :: commands          ! compute command queue (cl_command_queue)
      type(c_ptr) :: device            ! device we are using
      type(c_ptr) :: program           ! compute program
      type(c_ptr) :: event             ! event identifying the kernel execution instance
      logical     :: profiling         ! flag to enable profiling
      integer(c_int) :: elapsed        ! elapsed time in microseconds
   contains
      procedure, pass(this) :: init
      procedure, pass(this) :: setKernelArgInt
      procedure, pass(this) :: setKernelArgLoc
      procedure, pass(this) :: setKernelArgMem
      procedure, pass(this) :: setKernelArgReal
      procedure, pass(this) :: run
   end type CLKernel

   interface setKernelArg
      module procedure setKernelArgInt, setKernelArgMem, setKernelArgLoc, &
                       setKernelArgReal
   end interface

contains

   function init(this, context, commands, device, filename, name) result(status)
      implicit none
      class(CLKernel) :: this
      type(c_ptr) :: context, commands, device
      character(*) :: filename, name
      integer(cl_int) :: status

      integer(c_size_t) :: lengths(1)
      type(c_ptr) :: source

      integer(c_size_t) :: err_len
      integer, parameter :: err_buf_len = 2048
      character :: err_buf(err_buf_len)

      this%device = device
      this%commands = commands
      this%profiling = .false.
      this%elapsed = 0

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
      status = clBuildProgram(this%program, 0, C_NULL_PTR, C_NULL_CHAR, C_NULL_FUNPTR, C_NULL_PTR)
      if (status /= CL_SUCCESS) then

         print *, "CLKernel::init: Failed to build program executable!"
         status = clGetProgramBuildInfo(this%program, this%device, &
                                        CL_PROGRAM_BUILD_LOG, err_buf_len, err_buf, err_len)
!         printf("%s\n", buffer);
         call stop_on_error(status)
      end if

      ! Create the compute kernel in the program we wish to run
      !
      this%kernel = clCreateKernel(this%program, name, status)
      if (status /= CL_SUCCESS) then
         print *, "CLKernel::init: Failed to create compute kernel!"
         call stop_on_error(status)
      end if
   end function init

   function setKernelArgInt(this, argid, int_var) result(status)
      implicit none
      class(CLKernel) :: this
      integer, intent(in) :: argid
      integer(c_int) :: int_var
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
      class(CLKernel) :: this
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
      class(CLKernel) :: this
      integer, intent(in) :: argid
      type(c_ptr) :: cl_mem_obj
      integer(cl_int) :: status

      status = clSetKernelArg(this%kernel, argid, c_sizeof_cl_mem(), c_loc(cl_mem_obj))
      if (status /= CL_SUCCESS) then
         print *, "CLDevice::setKernelArgMem: Failed to set kernel argument!"
         call stop_on_error(status)
      end if
   end function setKernelArgMem

   function setKernelArgReal(this, argid, real_var) result(status)
      implicit none
      class(CLKernel) :: this
      integer, intent(in) :: argid
      real(c_float) :: real_var
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
      class(CLKernel) :: this
      integer(c_size_t) :: gWorkSizeX, gWorkSizeY, lWorkSizeX, lWorkSizeY
      integer(cl_int) :: status

      integer(c_size_t), dimension(2) :: global_work_offset, local_work_size, global_work_size
     
      global_work_offset = 0
      global_work_size = [gWorkSizeX, gWorkSizeY]
      local_work_size  = [lWorkSizeX, lWorkSizeY]

      ! execute the kernel
      !
      status = clEnqueueNDRangeKernel(this%commands, this%kernel, 2, global_work_offset, &
                                      global_work_size, local_work_size, 0, C_NULL_PTR, this%event)
      if (status /= CL_SUCCESS) then
         print *, "CLDevice::run(): Failed to execute kernel!"
         ! print *, "CLDevice::run(): max_local_work_size==%ld\n", max_local_size)
         call stop_on_error(status)
      end if

      ! wait for the command commands to get serviced before reading back results
      !
      status = clFinish(this%commands)

   end function run

end module CLKernel_mod
