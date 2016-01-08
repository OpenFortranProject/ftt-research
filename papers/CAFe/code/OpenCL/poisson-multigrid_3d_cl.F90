#define DUMP_OUTPUT
#undef DO_HALO_EXCHANGE
#undef DO_PROLONGATE
#define DO_RESTRICT
#define DO_RELAX

PROGRAM PoissonMultigrid
USE ForOpenCL
USE MultiGrid, ONLY: AddFourierMode_3D
USE IO, ONLY: Textual_Output_3D
USE MultiGrid, ONLY: Restrict_3D, Prolongate_3D
IMPLICIT NONE
REAL, PARAMETER :: w = (2.0/3.0)
INTEGER, PARAMETER :: N = 32
INTEGER, PARAMETER :: M = 32
INTEGER, PARAMETER :: L = 32
INTEGER, PARAMETER :: NP = 2
INTEGER, PARAMETER :: MP = 2
INTEGER, PARAMETER :: LP = 2
INTEGER, PARAMETER :: fd = 12

INTEGER :: t, i, device
INTEGER :: nsteps = 5

REAL, ALLOCATABLE, DIMENSION(:,:,:) :: V1h, V2h, V4h, V8h, Buf

TYPE(CLDevice) :: cl_device_
TYPE(CLBuffer) :: cl_V1h_
TYPE(CLBuffer) :: cl_Buf_
TYPE(CLBuffer) :: cl_V2h_
TYPE(CLBuffer) :: cl_V4h_
TYPE(CLBuffer) :: cl_V8h_
TYPE(CLKernel) :: cl_Relax_3D_
TYPE(CLKernel) :: cl_Restrict_3D_
TYPE(CLKernel) :: cl_Prolongate_3D_
INTEGER :: focl_intvar__
INTEGER(KIND=cl_int) :: cl_status__
INTEGER(KIND=c_size_t) :: cl_size__
INTEGER(KIND=c_size_t) :: cl_gwo__(3)
INTEGER(KIND=c_size_t) :: cl_gws__(3)
INTEGER(KIND=c_size_t) :: cl_lws__(3) = [32,8,1]

!! Device id
!
!  0 - CPU
!  1 - GPU1
!  2 - GPU2
!
integer :: device_id = 1
device = get_subimage(device_id,cl_device_)

print *, device, this_image(), num_images()

cl_status__ = query(cl_device_)

if (device == this_image()) then
   STOP "ERROR, device == this_image()"
end if

#ifdef DO_RELAX
cl_Relax_3D_ = createKernel(cl_device_,"Relax_3D")
#endif
#ifdef DO_RESTRICT
cl_Restrict_3D_ = createKernel(cl_device_,"Restrict_3D")
#endif
#ifdef DO_PROLONGATE
cl_Prolongate_3D_ = createKernel(cl_device_,"Prolongate_3D")
#endif


ALLOCATE(V1h(-1:N+1,-1:M+1,-1:L+1))
ALLOCATE(Buf(-1:N+1,-1:M+1,-1:L+1))
ALLOCATE(V2h(-1:N/2+1,-1:M/2+1,-1:L/2+1))
ALLOCATE(V4h(-1:N/4+1,-1:M/4+1,-1:L/4+1))
ALLOCATE(V8h(-1:N/8+1,-1:M/8+1,-1:L/8+1))

IF (device /= THIS_IMAGE()) THEN
  cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
  cl_V1h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
  cl_Buf_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N/2+1-(-1))+1)*((M/2+1-(-1))+1)*((L/2+1-(-1))+1)*1
  cl_V2h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N/4+1-(-1))+1)*((M/4+1-(-1))+1)*((L/4+1-(-1))+1)*1
  cl_V4h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N/8+1-(-1))+1)*((M/8+1-(-1))+1)*((L/8+1-(-1))+1)*1
  cl_V8h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
END IF

#ifdef DUMP_OUTPUT
OPEN(UNIT=fd, FILE="error_time.dat")
#endif

V1h = 0.0

CALL AddFourierMode_3D(N,M,L,V1h,1)
CALL AddFourierMode_3D(N,M,L,V1h,6)
CALL AddFourierMode_3D(N,M,L,V1h,16)

V1h = (1./3.)*V1h

cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
cl_status__ = writeBuffer(cl_V1h_,C_LOC(V1h),cl_size__)

#ifdef DUMP_OUTPUT
CALL Textual_Output_3D(N,M,L,V1h,"1h_0")
#endif

!! level 1h
!
DO t = 1, nsteps
#ifdef DO_RELAX
  cl_status__ = setKernelArg(cl_Relax_3D_,0,N)
  cl_status__ = setKernelArg(cl_Relax_3D_,1,M)
  cl_status__ = setKernelArg(cl_Relax_3D_,2,L)
  cl_status__ = setKernelArg(cl_Relax_3D_,3,clMemObject(cl_V1h_))
  cl_status__ = setKernelArg(cl_Relax_3D_,4,clMemObject(cl_Buf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N+1-(-1))+1),((M+1-(-1))+1),((L+1-(-1))+1)])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N+1-(-1))+1),((M+1-(-1))+1),((L+1-(-1))+1)])
  ! cl_gws__ = [32,24,24]
  cl_status__ = run(cl_Relax_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_3D_%commands)
#endif
#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(device,N,M,L,V1h)
#endif
END DO 

#ifdef DUMP_OUTPUT
cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V1h_,C_LOC(V1h),cl_size__)
WRITE(UNIT=fd,FMT=*) t, maxval(V1h)
CALL Textual_Output_3D(N,M,L,V1h,"1h_mid")
#endif

#ifdef DO_RESTRICT
cl_status__ = setKernelArg(cl_Restrict_3D_,0,N)
cl_status__ = setKernelArg(cl_Restrict_3D_,1,M)
cl_status__ = setKernelArg(cl_Restrict_3D_,2,L)
cl_status__ = setKernelArg(cl_Restrict_3D_,3,clMemObject(cl_V1h_))
cl_status__ = setKernelArg(cl_Restrict_3D_,4,clMemObject(cl_V2h_))
cl_gwo__ = [0,0,0]
cl_gws__ = [1,1,1]
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N+1-(-1))+1),((M+1-(-1))+1),((L+1-(-1))+1)])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/2+1-(-1))+1),((M/2+1-(-1))+1),((L/2+1-(-1))+1)])

! cl_gws__ = [32,24,24]
! cl_lws__ = [8,4,8] ! MAX APPEAR TO BE 2^8, 256 

!cl_status__ = run(cl_Restrict_3D_,2,cl_gwo__,cl_gws__,cl_lws__)
cl_status__ = run(cl_Restrict_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
cl_status__ = clFinish(cl_Restrict_3D_%commands)
#endif
#ifdef DUMP_OUTPUT
cl_size__ = 4*((N/2+1-(-1))+1)*((M/2+1-(-1))+1)*((L/2+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V2h_,C_LOC(V2h),cl_size__)
CALL Textual_Output_3D(N/2,M/2,L/2,V2h,"2h_0")
#endif

print *, "===FIN"
#undef DO_RESTRICT
#undef DO_PROLONGATE
#undef DO_RELAX
#undef DUMP_OUTPUT

!! level 2h
!
DO t = 1, nsteps
#ifdef DO_RELAX
  focl_intvar__ = N/2
  cl_status__ = setKernelArg(cl_Relax_3D_,0,focl_intvar__)
  focl_intvar__ = M/2
  cl_status__ = setKernelArg(cl_Relax_3D_,1,focl_intvar__)
  focl_intvar__ = L/2
  cl_status__ = setKernelArg(cl_Relax_3D_,2,focl_intvar__)
  cl_status__ = setKernelArg(cl_Relax_3D_,3,clMemObject(cl_V2h_))
  cl_status__ = setKernelArg(cl_Relax_3D_,4,clMemObject(cl_Buf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/2+1-(-1))+1),((M/2+1-(-1))+1),((L/2+1-(-1))+1)])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N+1-(-1))+1),((M+1-(-1))+1),((L+1-(-1))+1)])
  cl_status__ = run(cl_Relax_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_3D_%commands)
#endif
#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(device,N/2,M/2,L/2,V2h)
#endif
END DO 

#ifdef DUMP_OUTPUT
cl_size__ = 4*((N/2+1-(-1))+1)*((M/2+1-(-1))+1)*((L/2+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V2h_,C_LOC(V2h),cl_size__)
WRITE(UNIT=fd,FMT=*) t, maxval(V2h)
CALL Textual_Output_3D(N/2,M/2,L/2,V2h,"2h_mid")
#endif

#ifdef DO_RESTRICT
focl_intvar__ = N/2
cl_status__ = setKernelArg(cl_Restrict_3D_,0,focl_intvar__)
focl_intvar__ = M/2
cl_status__ = setKernelArg(cl_Restrict_3D_,1,focl_intvar__)
focl_intvar__ = L/2
cl_status__ = setKernelArg(cl_Restrict_3D_,2,focl_intvar__)
cl_status__ = setKernelArg(cl_Restrict_3D_,3,clMemObject(cl_V2h_))
cl_status__ = setKernelArg(cl_Restrict_3D_,4,clMemObject(cl_V4h_))
cl_gwo__ = [0,0,0]
cl_gws__ = [1,1,1]
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/2+1-(-1))+1),((M/2+1-(-1))+1),((L/2+1-(-1))+1)])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/4+1-(-1))+1),((M/4+1-(-1))+1),((L/4+1-(-1))+1)])
cl_status__ = run(cl_Restrict_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
cl_status__ = clFinish(cl_Restrict_3D_%commands)
#endif
#ifdef DUMP_OUTPUT
cl_size__ = 4*((N/4+1-(-1))+1)*((M/4+1-(-1))+1)*((L/4+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V4h_,C_LOC(V4h),cl_size__)
CALL Textual_Output_3D(N/4,M/4,L/4,V4h,"4h_0")
#endif

!! level 4h
!
DO t = 1, nsteps
#ifdef DO_RELAX
  focl_intvar__ = N/4
  cl_status__ = setKernelArg(cl_Relax_3D_,0,focl_intvar__)
  focl_intvar__ = M/4
  cl_status__ = setKernelArg(cl_Relax_3D_,1,focl_intvar__)
  focl_intvar__ = L/4
  cl_status__ = setKernelArg(cl_Relax_3D_,2,focl_intvar__)
  cl_status__ = setKernelArg(cl_Relax_3D_,3,clMemObject(cl_V4h_))
  cl_status__ = setKernelArg(cl_Relax_3D_,4,clMemObject(cl_Buf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/4+1-(-1))+1),((M/4+1-(-1))+1),((L/4+1-(-1))+1)])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N+1-(-1))+1),((M+1-(-1))+1),((L+1-(-1))+1)])
  cl_status__ = run(cl_Relax_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_3D_%commands)
#endif
#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(device,N/4,M/4,L/4,V4h)
#endif
END DO 

#ifdef DUMP_OUTPUT
cl_size__ = 4*((N/4+1-(-1))+1)*((M/4+1-(-1))+1)*((L/4+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V4h_,C_LOC(V4h),cl_size__)
WRITE(UNIT=fd,FMT=*) t, maxval(V4h)
CALL Textual_Output_3D(N/4,M/4,L/4,V4h,"4h_mid")
#endif

V8h = 0

cl_size__ = 4*((N/8+1-(-1))+1)*((M/8+1-(-1))+1)*((L/8+1-(-1))+1)*1
cl_status__ = writeBuffer(cl_V8h_,C_LOC(V8h),cl_size__)

#ifdef DO_PROLONGATE
focl_intvar__ = N/4
cl_status__ = setKernelArg(cl_Prolongate_3D_,0,focl_intvar__)
focl_intvar__ = M/4
cl_status__ = setKernelArg(cl_Prolongate_3D_,1,focl_intvar__)
focl_intvar__ = L/4
cl_status__ = setKernelArg(cl_Prolongate_3D_,2,focl_intvar__)
cl_status__ = setKernelArg(cl_Prolongate_3D_,3,clMemObject(cl_V4h_))
cl_status__ = setKernelArg(cl_Prolongate_3D_,4,clMemObject(cl_V8h_))
cl_gwo__ = [0,0,0]
cl_gws__ = [1,1,1]
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/4+1-(-1))+1),((M/4+1-(-1))+1),((L/4+1-(-1))+1)])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/8+1-(-1))+1),((M/8+1-(-1))+1),((L/8+1-(-1))+1)])
cl_status__ = run(cl_Prolongate_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
cl_status__ = clFinish(cl_Prolongate_3D_%commands)
#endif

!! level 4h
!
DO t = 1, nsteps
#ifdef DO_RELAX
  focl_intvar__ = N/4
  cl_status__ = setKernelArg(cl_Relax_3D_,0,focl_intvar__)
  focl_intvar__ = M/4
  cl_status__ = setKernelArg(cl_Relax_3D_,1,focl_intvar__)
  focl_intvar__ = L/4
  cl_status__ = setKernelArg(cl_Relax_3D_,2,focl_intvar__)
  cl_status__ = setKernelArg(cl_Relax_3D_,3,clMemObject(cl_V4h_))
  cl_status__ = setKernelArg(cl_Relax_3D_,4,clMemObject(cl_Buf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/4+1-(-1))+1),((M/4+1-(-1))+1),((L/4+1-(-1))+1)])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N+1-(-1))+1),((M+1-(-1))+1),((L+1-(-1))+1)])
  cl_status__ = run(cl_Relax_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_3D_%commands)
#endif
#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(device,N/4,M/4,L/4,V4h)
#endif
END DO 

#ifdef DUMP_OUTPUT
cl_size__ = 4*((N/4+1-(-1))+1)*((M/4+1-(-1))+1)*((L/4+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V4h_,C_LOC(V4h),cl_size__)
WRITE(UNIT=fd,FMT=*) t, maxval(V4h)
CALL Textual_Output_3D(N/4,M/4,L/4,V4h,"4h_end")
#endif

#ifdef DO_PROLONGATE
focl_intvar__ = N/2
cl_status__ = setKernelArg(cl_Prolongate_3D_,0,focl_intvar__)
focl_intvar__ = M/2
cl_status__ = setKernelArg(cl_Prolongate_3D_,1,focl_intvar__)
focl_intvar__ = L/2
cl_status__ = setKernelArg(cl_Prolongate_3D_,2,focl_intvar__)
cl_status__ = setKernelArg(cl_Prolongate_3D_,3,clMemObject(cl_V2h_))
cl_status__ = setKernelArg(cl_Prolongate_3D_,4,clMemObject(cl_V4h_))
cl_gwo__ = [0,0,0]
cl_gws__ = [1,1,1]
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/2+1-(-1))+1),((M/2+1-(-1))+1),((L/2+1-(-1))+1)])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/4+1-(-1))+1),((M/4+1-(-1))+1),((L/4+1-(-1))+1)])
cl_status__ = run(cl_Prolongate_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
cl_status__ = clFinish(cl_Prolongate_3D_%commands)
#endif

!! level 2h
!
DO t = 1, nsteps
#ifdef DO_RELAX
  focl_intvar__ = N/2
  cl_status__ = setKernelArg(cl_Relax_3D_,0,focl_intvar__)
  focl_intvar__ = M/2
  cl_status__ = setKernelArg(cl_Relax_3D_,1,focl_intvar__)
  focl_intvar__ = L/2
  cl_status__ = setKernelArg(cl_Relax_3D_,2,focl_intvar__)
  cl_status__ = setKernelArg(cl_Relax_3D_,3,clMemObject(cl_V2h_))
  cl_status__ = setKernelArg(cl_Relax_3D_,4,clMemObject(cl_Buf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/2+1-(-1))+1),((M/2+1-(-1))+1),((L/2+1-(-1))+1)])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N+1-(-1))+1),((M+1-(-1))+1),((L+1-(-1))+1)])
  cl_status__ = run(cl_Relax_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_3D_%commands)
#endif
#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(device,N/2,M/2,L/2,V2h)
#endif
END DO 

#ifdef DUMP_OUTPUT
cl_size__ = 4*((N/2+1-(-1))+1)*((M/2+1-(-1))+1)*((L/2+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V2h_,C_LOC(V2h),cl_size__)
WRITE(UNIT=fd,FMT=*) t, maxval(V2h)
CALL Textual_Output_3D(N/2,M/2,L/2,V2h,"2h_end")
#endif

#ifdef DO_PROLONGATE
cl_status__ = setKernelArg(cl_Prolongate_3D_,0,N)
cl_status__ = setKernelArg(cl_Prolongate_3D_,1,M)
cl_status__ = setKernelArg(cl_Prolongate_3D_,2,L)
cl_status__ = setKernelArg(cl_Prolongate_3D_,3,clMemObject(cl_V1h_))
cl_status__ = setKernelArg(cl_Prolongate_3D_,4,clMemObject(cl_V2h_))
cl_gwo__ = [0,0,0]
cl_gws__ = [1,1,1]
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N+1-(-1))+1),((M+1-(-1))+1),((L+1-(-1))+1)])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/2+1-(-1))+1),((M/2+1-(-1))+1),((L/2+1-(-1))+1)])
cl_status__ = run(cl_Prolongate_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
cl_status__ = clFinish(cl_Prolongate_3D_%commands)
#endif

!! level 1h
!
DO t = 1, nsteps
#ifdef DO_RELAX
  cl_status__ = setKernelArg(cl_Relax_3D_,0,N)
  cl_status__ = setKernelArg(cl_Relax_3D_,1,M)
  cl_status__ = setKernelArg(cl_Relax_3D_,2,L)
  cl_status__ = setKernelArg(cl_Relax_3D_,3,clMemObject(cl_V1h_))
  cl_status__ = setKernelArg(cl_Relax_3D_,4,clMemObject(cl_Buf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N+1-(-1))+1),((M+1-(-1))+1),((L+1-(-1))+1)])
  cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N+1-(-1))+1),((M+1-(-1))+1),((L+1-(-1))+1)])
  cl_status__ = run(cl_Relax_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_Relax_3D_%commands)
#endif
#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(device,N,M,L,V1h)
#endif
END DO 

#ifdef DUMP_OUTPUT
cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V1h_,C_LOC(V1h),cl_size__)
WRITE(UNIT=fd,FMT=*) t, maxval(V1h)
CALL Textual_Output_3D(N,M,L,V1h,"1h_end")
CLOSE(UNIT=fd)
#endif

END PROGRAM PoissonMultigrid
