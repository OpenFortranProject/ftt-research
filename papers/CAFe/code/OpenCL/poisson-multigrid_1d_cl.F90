PROGRAM PoissonMultigrid
USE ForOpenCL
USE MultiGrid, ONLY: AddFourierMode
USE IO, ONLY: Textual_Output
USE MultiGrid, ONLY: Restrict_1D, Prolongate_1D
IMPLICIT NONE
REAL, PARAMETER :: w = (2.0/3.0)
INTEGER, PARAMETER :: N = 64
INTEGER, PARAMETER :: fd = 12
INTEGER :: t, i, device
INTEGER :: nsteps = 5
REAL, ALLOCATABLE, DIMENSION(:) :: V1h, V2h, V4h, V8h, Buf
TYPE(CLDevice) :: cl_device_
TYPE(CLBuffer) :: cl_V1h_
TYPE(CLBuffer) :: cl_Buf_
TYPE(CLBuffer) :: cl_V2h_
TYPE(CLBuffer) :: cl_V4h_
TYPE(CLBuffer) :: cl_V8h_
TYPE(CLKernel) :: cl_Relax_1D_
TYPE(CLKernel) :: cl_Relax_
INTEGER(KIND=cl_int) :: cl_status__
INTEGER(KIND=c_size_t) :: cl_size__
INTEGER(KIND=c_size_t) :: cl_gwo__(3)
INTEGER(KIND=c_size_t) :: cl_gws__(3)
INTEGER(KIND=c_size_t) :: cl_lws__(3) = [1,1,1]
device = get_subimage(0,cl_device_)
ALLOCATE(V1h(-1:N+1))
ALLOCATE(Buf(-1:N+1))
ALLOCATE(V2h(-1:N/2+1))
ALLOCATE(V4h(-1:N/4+1))
ALLOCATE(V8h(-1:N/8+1))
IF (device /= THIS_IMAGE()) THEN
  cl_size__ = 4*((N+1-(-1))+1)*1
  cl_V1h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N+1-(-1))+1)*1
  cl_Buf_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N/2+1-(-1))+1)*1
  cl_V2h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N/4+1-(-1))+1)*1
  cl_V4h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N/8+1-(-1))+1)*1
  cl_V8h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
END IF
OPEN(UNIT=fd, FILE="error_time.dat")
V1h = 0.0
CALL AddFourierMode(N,V1h,1)
CALL AddFourierMode(N,V1h,6)
CALL AddFourierMode(N,V1h,16)
V1h = (1./3.)*V1h

cl_size__ = 4*((N+1-(-1))+1)*1
cl_status__ = writeBuffer(cl_V1h_,C_LOC(V1h),cl_size__)

cl_Relax_1D_ = createKernel(cl_device_,"Relax_1D")
cl_Relax_    = createKernel(cl_device_,"Relax")

CALL Textual_Output(N,V1h,"1h_0")

cl_status__ = setKernelArg(cl_Relax_1D_,0,N)
cl_status__ = setKernelArg(cl_Relax_1D_,1,clMemObject(cl_V1h_))
cl_status__ = setKernelArg(cl_Relax_1D_,2,clMemObject(cl_Buf_))
cl_status__ = setKernelArg(cl_Relax_,1,clMemObject(cl_V2h_))
cl_status__ = setKernelArg(cl_Relax_,2,clMemObject(cl_Buf_))

DO t = 1, nsteps
  cl_gwo__(1) = 0-1
  cl_gws__(1) = ((N-(0))+1)
  cl_status__ = run(cl_Relax_1D_,1,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_1D_%commands)
  CALL Exchange_Halo(device,N,V1h)
  WRITE(UNIT=fd,FMT=*) t, maxval(V1h)
END DO 

cl_size__ = 4*((N+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V1h_,C_LOC(V1h),cl_size__)
CALL Textual_Output(N,V1h,"1h_mid")

CALL Restrict_1D(N,V1h,V2h)
CALL Textual_Output(N/2,V2h,"2h_0")
DO t = 1, nsteps
  cl_Relax_1D_ = createKernel(cl_device_,"Relax_1D")
  cl_status__ = setKernelArg(cl_Relax_1D_,0,N)
  cl_status__ = setKernelArg(cl_Relax_1D_,1,V1h)
  cl_status__ = setKernelArg(cl_Relax_1D_,2,Buf)
  cl_Relax_ = createKernel(cl_device_,"Relax")
  cl_status__ = setKernelArg(cl_Relax_,1,V2h)
  cl_status__ = setKernelArg(cl_Relax_,2,Buf)
  cl_gwo__(1) = 0-1
  cl_gws__(1) = ((N-(0))+1)
  cl_status__ = run(cl_Relax_,1,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_%commands)
  CALL Exchange_Halo(device,N/2,V2h)
  WRITE(UNIT=fd,FMT=*) t, maxval(V2h)
END DO 
cl_size__ = 4*((N/2+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V2h_,C_LOC(V2h),cl_size__)
CALL Textual_Output(N/2,V2h,"2h_mid")
CALL Restrict_1D(N/2,V2h,V4h)
CALL Textual_Output(N/4,V4h,"4h_0")
DO t = 1, nsteps
  cl_Relax_1D_ = createKernel(cl_device_,"Relax_1D")
  cl_status__ = setKernelArg(cl_Relax_1D_,0,N)
  cl_status__ = setKernelArg(cl_Relax_1D_,1,V1h)
  cl_status__ = setKernelArg(cl_Relax_1D_,2,Buf)
  cl_Relax_ = createKernel(cl_device_,"Relax")
  cl_status__ = setKernelArg(cl_Relax_,1,V2h)
  cl_status__ = setKernelArg(cl_Relax_,2,Buf)
  cl_gwo__(1) = 0-1
  cl_gws__(1) = ((N-(0))+1)
  cl_status__ = run(cl_Relax_,1,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_%commands)
  CALL Exchange_Halo(device,N/4,V4h)
  WRITE(UNIT=fd,FMT=*) t, maxval(V4h)
END DO 
cl_size__ = 4*((N/4+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V4h_,C_LOC(V4h),cl_size__)
CALL Textual_Output(N/4,V4h,"4h_mid")
CALL Restrict_1D(N/4,V4h,V8h)
CALL Textual_Output(N/8,V8h,"8h_0")
DO t = 1, nsteps
  cl_Relax_1D_ = createKernel(cl_device_,"Relax_1D")
  cl_status__ = setKernelArg(cl_Relax_1D_,0,N)
  cl_status__ = setKernelArg(cl_Relax_1D_,1,V1h)
  cl_status__ = setKernelArg(cl_Relax_1D_,2,Buf)
  cl_Relax_ = createKernel(cl_device_,"Relax")
  cl_status__ = setKernelArg(cl_Relax_,1,V2h)
  cl_status__ = setKernelArg(cl_Relax_,2,Buf)
  cl_gwo__(1) = 0-1
  cl_gws__(1) = ((N-(0))+1)
  cl_status__ = run(cl_Relax_,1,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_%commands)
  CALL Exchange_Halo(device,N/8,V8h)
  WRITE(UNIT=fd,FMT=*) t, maxval(V8h)
END DO 
cl_size__ = 4*((N/8+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V8h_,C_LOC(V8h),cl_size__)
CALL Textual_Output(N/8,V8h,"8h_mid")
Buf = 0
cl_size__ = 4*((N+1-(-1))+1)*1
cl_status__ = writeBuffer(cl_Buf_,C_LOC(Buf),cl_size__)
CALL Prolongate_1D(N/8,V8h,Buf)
DO t = 1, nsteps
  cl_Relax_1D_ = createKernel(cl_device_,"Relax_1D")
  cl_status__ = setKernelArg(cl_Relax_1D_,0,N)
  cl_status__ = setKernelArg(cl_Relax_1D_,1,V1h)
  cl_status__ = setKernelArg(cl_Relax_1D_,2,Buf)
  cl_Relax_ = createKernel(cl_device_,"Relax")
  cl_status__ = setKernelArg(cl_Relax_,1,V2h)
  cl_status__ = setKernelArg(cl_Relax_,2,Buf)
  cl_gwo__(1) = 0-1
  cl_gws__(1) = ((N-(0))+1)
  cl_status__ = run(cl_Relax_,1,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_%commands)
  CALL Exchange_Halo(device,N/8,V8h)
  WRITE(UNIT=fd,FMT=*) t, maxval(V8h)
END DO 
cl_size__ = 4*((N/8+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V8h_,C_LOC(V8h),cl_size__)
CALL Textual_Output(N/8,V8h,"8h_end")
CALL Prolongate_1D(N/4,V4h,V8h)
DO t = 1, nsteps
  cl_Relax_1D_ = createKernel(cl_device_,"Relax_1D")
  cl_status__ = setKernelArg(cl_Relax_1D_,0,N)
  cl_status__ = setKernelArg(cl_Relax_1D_,1,V1h)
  cl_status__ = setKernelArg(cl_Relax_1D_,2,Buf)
  cl_Relax_ = createKernel(cl_device_,"Relax")
  cl_status__ = setKernelArg(cl_Relax_,1,V2h)
  cl_status__ = setKernelArg(cl_Relax_,2,Buf)
  cl_gwo__(1) = 0-1
  cl_gws__(1) = ((N-(0))+1)
  cl_status__ = run(cl_Relax_,1,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_%commands)
  CALL Exchange_Halo(device,N/4,V4h)
  WRITE(UNIT=fd,FMT=*) t, maxval(V4h)
END DO 
cl_size__ = 4*((N/4+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V4h_,C_LOC(V4h),cl_size__)
CALL Textual_Output(N/4,V4h,"4h_end")
CALL Prolongate_1D(N/2,V2h,V4h)
DO t = 1, nsteps
  cl_Relax_1D_ = createKernel(cl_device_,"Relax_1D")
  cl_status__ = setKernelArg(cl_Relax_1D_,0,N)
  cl_status__ = setKernelArg(cl_Relax_1D_,1,V1h)
  cl_status__ = setKernelArg(cl_Relax_1D_,2,Buf)
  cl_Relax_ = createKernel(cl_device_,"Relax")
  cl_status__ = setKernelArg(cl_Relax_,1,V2h)
  cl_status__ = setKernelArg(cl_Relax_,2,Buf)
  cl_gwo__(1) = 0-1
  cl_gws__(1) = ((N-(0))+1)
  cl_status__ = run(cl_Relax_,1,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_%commands)
  CALL Exchange_Halo(device,N/2,V2h)
  WRITE(UNIT=fd,FMT=*) t, maxval(V2h)
END DO 
cl_size__ = 4*((N/2+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V2h_,C_LOC(V2h),cl_size__)
CALL Textual_Output(N/2,V2h,"2h_end")
CALL Prolongate_1D(N,V1h,V2h)
DO t = 1, nsteps
  cl_Relax_1D_ = createKernel(cl_device_,"Relax_1D")
  cl_status__ = setKernelArg(cl_Relax_1D_,0,N)
  cl_status__ = setKernelArg(cl_Relax_1D_,1,V1h)
  cl_status__ = setKernelArg(cl_Relax_1D_,2,Buf)
  cl_Relax_ = createKernel(cl_device_,"Relax")
  cl_status__ = setKernelArg(cl_Relax_,1,V2h)
  cl_status__ = setKernelArg(cl_Relax_,2,Buf)
  cl_gwo__(1) = 0-1
  cl_gws__(1) = ((N-(0))+1)
  cl_status__ = run(cl_Relax_1D_,1,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_1D_%commands)
  CALL Exchange_Halo(device,N,V1h)
  WRITE(UNIT=fd,FMT=*) t, maxval(V1h)
END DO 
cl_size__ = 4*((N+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V1h_,C_LOC(V1h),cl_size__)
CALL Textual_Output(N,V1h,"1h_end")
CLOSE(UNIT=fd)

CONTAINS

SUBROUTINE Put_Halo(device,N,A)
USE ForOpenCL
TYPE(CLDevice), INTENT(IN) :: device
INTEGER, INTENT(IN) :: N
REAL, INTENT(INOUT) :: A(-1:N+1)    ! [*]
TYPE(CLDevice) :: cl_device_
TYPE(CLBuffer) :: cl_V1h_
TYPE(CLBuffer) :: cl_Buf_
TYPE(CLBuffer) :: cl_V2h_
TYPE(CLBuffer) :: cl_V4h_
TYPE(CLBuffer) :: cl_V8h_
TYPE(CLKernel) :: cl_Relax_1D_
TYPE(CLKernel) :: cl_Relax_
INTEGER(KIND=cl_int) :: cl_status__
INTEGER(KIND=c_size_t) :: cl_size__
INTEGER(KIND=c_size_t) :: cl_gwo__(3)
INTEGER(KIND=c_size_t) :: cl_gws__(3)
INTEGER(KIND=c_size_t) :: cl_lws__(3) = [1,1,1]

!A(-1:0)[device] = A(-1:0)
!A(N:N+1)[device] = A(N:N+1)

END SUBROUTINE Put_Halo

SUBROUTINE Get_Halo(device,N,A)
USE ForOpenCL
TYPE(CLDevice), INTENT(IN) :: device
INTEGER, INTENT(IN) :: N
REAL, INTENT(INOUT) :: A(-1:N+1)     ! [*]
TYPE(CLDevice) :: cl_device_
TYPE(CLBuffer) :: cl_V1h_
TYPE(CLBuffer) :: cl_Buf_
TYPE(CLBuffer) :: cl_V2h_
TYPE(CLBuffer) :: cl_V4h_
TYPE(CLBuffer) :: cl_V8h_
TYPE(CLKernel) :: cl_Relax_1D_
TYPE(CLKernel) :: cl_Relax_
INTEGER(KIND=cl_int) :: cl_status__
INTEGER(KIND=c_size_t) :: cl_size__
INTEGER(KIND=c_size_t) :: cl_gwo__(3)
INTEGER(KIND=c_size_t) :: cl_gws__(3)
INTEGER(KIND=c_size_t) :: cl_lws__(3) = [1,1,1]

!A(-1:0) = A(-1:0)[device]
!A(N:N+1) = A(N:N+1)[device]

END SUBROUTINE Get_Halo

#ifdef USE_CAF
Subroutine Exchange_Halo(N, A)
!
! Exchange halo information between neighboring processes
!
   Implicit None
   Integer, intent(in   ) :: N
   Real,    intent(inout) :: A(-1:N+1)    ! [*]

   integer :: left, right

   left  = THIS_IMAGE() - 1
   right = THIS_IMAGE() + 1

   if (left  < 1)             left  = NUM_IMAGES()
   if (right > NUM_IMAGES())  right = 1

   !! Exchange with neighbors (copy)
   !
   A( -1) = A(N-1) [left ]
   A(N+1) = A(  1) [right]

   !! Compute common boundaries now that we have halo computed at complete relax step
   !
   A(0) = (1.0 - w)*A(0) + 0.5*w*(A( -1) + A( +1))
   A(N) = (1.0 - w)*A(N) + 0.5*w*(A(N-1) + A(N+1))

   !! Reset boundary conditions (in case they've changed, why should they?)
   !
   if (THIS_IMAGE() == 1           ) A(0) = 0.0
   if (THIS_IMAGE() == NUM_IMAGES()) A(N) = 0.0

End Subroutine Exchange_Halo
#endif

END PROGRAM PoissonMultigrid
