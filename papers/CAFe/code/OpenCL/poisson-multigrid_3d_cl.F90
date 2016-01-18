#undef USE_MPI

#undef DUMP_OUTPUT
#undef DO_HALO_EXCHANGE
#undef DO_PROLONGATE
#undef DO_RESTRICT
#define DO_RELAX
#define DO_GETBOUNDARY
#undef VERBOSE

PROGRAM PoissonMultigrid

#ifdef USE_MPI
Use Parallel
#endif

USE ForOpenCL
USE Timer_mod
USE MultiGrid, ONLY: AddFourierMode_3D, Exchange_Halo_3D, RelaxBoundary_3D
USE MultiGrid, ONLY: Copyto_Halo_Buf_3D, Copyfrom_Halo_Buf_3D
USE IO, ONLY: Textual_Output_3D
IMPLICIT NONE
REAL, PARAMETER :: w = (2.0/3.0)
INTEGER, PARAMETER :: N = 4
INTEGER, PARAMETER :: M = 4
INTEGER, PARAMETER :: L = 4
INTEGER, PARAMETER :: fd = 12

INTEGER            :: NP      ! let this vary as needed
INTEGER, PARAMETER :: MP = 1
INTEGER, PARAMETER :: LP = 1

INTEGER :: npx, npy, npz      ! number of actual processors in x,y,z dimensions
INTEGER ::  ex,  ey,  ez      ! ending indices of interior region (start is 1)

INTEGER :: t, i, j, k, device
INTEGER :: nsteps = 100

REAL, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: V1h, V2h, V4h, V8h, Buf
REAL, ALLOCATABLE, TARGET, DIMENSION(:)     :: BoundaryBuf, RecvBuf

TYPE(CLDevice) :: cl_device_
TYPE(CLBuffer) :: cl_V1h_
TYPE(CLBuffer) :: cl_Buf_
TYPE(CLBuffer) :: cl_BoundaryBuf_
TYPE(CLBuffer) :: cl_V2h_
TYPE(CLBuffer) :: cl_V4h_
TYPE(CLBuffer) :: cl_V8h_
TYPE(CLKernel) :: cl_Relax_3D_
TYPE(CLKernel) :: cl_Restrict_3D_
TYPE(CLKernel) :: cl_Prolongate_3D_
TYPE(CLKernel) :: cl_GetBoundary_3D_
INTEGER :: focl_intvar__
INTEGER(KIND=cl_int) :: cl_status__
INTEGER(KIND=c_size_t) :: cl_size__, cl_buf_size__
INTEGER(KIND=c_size_t) :: cl_gwo__(3)
INTEGER(KIND=c_size_t) :: cl_gws__(3)
INTEGER(KIND=c_size_t) :: cl_lws__(3) = [1,1,1] ![32,4,1]

TYPE(CPUTimer) :: gpu_timer, cpu_timer, transfer_timer
REAL(KIND=c_double) :: cpu_time, gpu_time, transfer_time
REAL(KIND=c_double) :: total_cpu_time, total_gpu_time, total_transfer_time

integer :: device_id = 1
integer :: o               ! array offset
integer :: num             ! number of elements

total_cpu_time = 0
total_gpu_time = 0
total_transfer_time = 0
#ifdef USE_MPI
  call Parallel_Start

  if (numproc < MP*LP) then
     STOP 'ERROR: MPI size must be greater than MP*LP'
  end if

  NP = numproc/(MP*LP)
  npex = NP
  npey = MP
  npez = LP

  call Topology

  npx = npex
  npy = npey
  npz = npez

  print *, my_id, 'r:', npx, npy, npz
  print *, my_id, 'x:', left, right
  print *, my_id, 'y:', bottom, top
  print *, my_id, 'z:', front, back
  print *
#endif

!! Device id
!
!  0 - CPU
!  1 - GPU1
!  2 - GPU2
!
device = get_subimage(device_id,cl_device_)

!! May want information about the devices
!cl_status__ = query(cl_device_)

!! No coarrays
!
!if (device == this_image()) then
!   STOP "ERROR, device == this_image()"
!end if

#ifdef DO_RELAX
cl_Relax_3D_ = createKernel(cl_device_,"Relax_3D")
#endif
#ifdef DO_RESTRICT
cl_Restrict_3D_ = createKernel(cl_device_,"Restrict_3D")
#endif
#ifdef DO_PROLONGATE
cl_Prolongate_3D_ = createKernel(cl_device_,"Prolongate_3D")
#endif
#ifdef DO_GETBOUNDARY
cl_GetBoundary_3D_ = createKernel(cl_device_,"GetBoundary_3D")
#endif

ALLOCATE(V1h(-1:N+1,-1:M+1,-1:L+1))
ALLOCATE(Buf(-1:N+1,-1:M+1,-1:L+1))
ALLOCATE(BoundaryBuf(2*(M-1)*(L-1) + 2*(N-1)*(L-1) + 2*(N-1)*(M-1))) ! 6 boundary planes for device
ALLOCATE(    RecvBuf(2*(M-1)*(L-1) + 2*(N-1)*(L-1) + 2*(N-1)*(M-1))) ! 6 boundary planes for neighbors
ALLOCATE(V2h(-1:N/2+1,-1:M/2+1,-1:L/2+1))
ALLOCATE(V4h(-1:N/4+1,-1:M/4+1,-1:L/4+1))
ALLOCATE(V8h(-1:N/8+1,-1:M/8+1,-1:L/8+1))

!! No coarrays
!
!IF (device /= THIS_IMAGE()) THEN
  cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
  cl_V1h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
  cl_Buf_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_buf_size__ = 4*(2*(M-1)*(L-1) + 2*(N-1)*(L-1) + 2*(N-1)*(M-1))
  cl_BoundaryBuf_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_buf_size__,C_NULL_PTR)
  cl_size__ = 4*((N/2+1-(-1))+1)*((M/2+1-(-1))+1)*((L/2+1-(-1))+1)*1
  cl_V2h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N/4+1-(-1))+1)*((M/4+1-(-1))+1)*((L/4+1-(-1))+1)*1
  cl_V4h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
  cl_size__ = 4*((N/8+1-(-1))+1)*((M/8+1-(-1))+1)*((L/8+1-(-1))+1)*1
  cl_V8h_ = createBuffer(cl_device_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
!END IF

#ifdef DUMP_OUTPUT
OPEN(UNIT=fd, FILE="error_time.dat")
#endif

V1h = 0.0
V2h = 0.0

!USE_MPI - need to use Williams changes to add domain decomposition for initialization
!        - but ignore for now and let each image compute exactly same information
!        - this will mean less problems for boundary conditions, we just set them 0 on all ranks
!
CALL AddFourierMode_3D(N,M,L,V1h,1)
CALL AddFourierMode_3D(N,M,L,V1h,6)
CALL AddFourierMode_3D(N,M,L,V1h,16)

V1h = (1./3.)*V1h

! V1h(-1:0,:,:) = -6
! V1h(:,-1:0,:) = -6
! V1h(:,:,-1:0) = -6
! V1h(N:N+1,:,:) = -6
! V1h(:,N:N+1,:) = -6
! V1h(:,:,N:N+1) = -6

! do i = -1,N+1 
!    do j = -1,M+1
!       do k = -1,L+1
!          V1h(i,j,k) = 900000 + (i+1)*10000 + (1+j)*100 + k+1
!       end do
!    end do
! end do

! V2h(-1:0,:,:) = -6
! V2h(:,-1:0,:) = -6
! V2h(:,:,-1:0) = -6
! V2h(N/2:N/2+1,:,:) = -6
! V2h(:,N/2:N/2+1,:) = -6
! V2h(:,:,N/2:N/2+1) = -6
#ifdef VERBOSE
print *, "Start"
print *, V1h
#endif

cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
cl_status__ = writeBuffer(cl_V1h_,C_LOC(V1h),cl_size__)

cl_size__ = 4*((N/2+1-(-1))+1)*((M/2+1-(-1))+1)*((L/2+1-(-1))+1)*1
cl_status__ = writeBuffer(cl_V2h_,C_LOC(V2h),cl_size__)

BoundaryBuf = 0
cl_status__ = writeBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_buf_size__)

#ifdef DUMP_OUTPUT
#ifdef USE_MPI
if (my_id == 0) then
   !USE_MPI - need to gather?  Or add rank to output file (probably the easiest)
   CALL Textual_Output_3D(N,M,L,V1h,"1h_0")
end if
#endif
#endif

print *
print *, "Measuring flops and effective bandwidth for GPU computation:"

call init(gpu_timer)
call init(cpu_timer)
call init(transfer_timer)
!! level 1h
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO t = 1, nsteps
#ifdef DO_RELAX

  call start(gpu_timer)
  cl_status__ = setKernelArg(cl_Relax_3D_,0,N)
  cl_status__ = setKernelArg(cl_Relax_3D_,1,M)
  cl_status__ = setKernelArg(cl_Relax_3D_,2,L)
  cl_status__ = setKernelArg(cl_Relax_3D_,3,clMemObject(cl_V1h_))
  cl_status__ = setKernelArg(cl_Relax_3D_,4,clMemObject(cl_Buf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [N,M,L]
#ifdef VERBOSE
  print *, "RELAX gwx and lws"
  print *, cl_gws__
  print *, cl_lws__
#endif
  cl_status__ = run(cl_Relax_3D_,3,cl_gwo__,cl_gws__,cl_lws__)

  ! relax shared boundaries
  call start(cpu_timer)
  call RelaxBoundary_3D(N,M,L,V1h)
  call stop(cpu_timer)

  cl_status__ = clFinish(cl_Relax_3D_%commands)
#endif

#ifdef VERBOSE
  print *, "After RELAX V1h", V1h(:,1:2,:)
#endif
call stop(gpu_timer)

! gpu_time = elapsed_time(cl_Relax_3D_%timer)
! print *, " submit time    ==   ", real(gpu_time) 
! total_gpu_time = elapsed_time(timer) + total_gpu_time

! PRINT THE RUNNING AVERAGE
! print *, " elapsed cpu time    ==   ", elapsed_time(cpu_timer) / t
! print *, " elapsed gpu time    ==   ", (elapsed_time(gpu_timer)-elapsed_time(cpu_timer)) / t


#ifdef DO_GETBOUNDARY

call start(transfer_timer)

  ex = N-1
  ey = M-1
  ez = L-1

  !! put boundaries to device, indices (0,N) ...
  !
  call Copyto_Halo_Buf_3D(N, M, L, V1h, BoundaryBuf)

#ifdef VERBOSE
  print *, "-----BB------"
  print *, int(BoundaryBuf( 0+1: 0+ey*ez))
  print *, int(BoundaryBuf( 9+1: 9+ey*ez))
  print *, int(BoundaryBuf(18+1:18+ex*ez))
  print *, int(BoundaryBuf(27+1:27+ex*ez))
  print *, int(BoundaryBuf(36+1:36+ex*ey))
  print *, int(BoundaryBuf(45+1:45+ex*ey))
  print *, "-----------------"
#endif
  
  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = writeBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  !! run copy memory kernel on device
  !
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,0,N)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,1,M)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,2,L)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,3,clMemObject(cl_V1h_))
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,4,clMemObject(cl_BoundaryBuf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [ey*ez + ex*ez + ex*ey,2,1]
  cl_status__ = run(cl_GetBoundary_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_GetBoundary_3D_%commands)

  !! get boundaries from device (1,N-1)
  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = readBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  call Copyfrom_Halo_Buf_3D(N, M, L, V1h, BoundaryBuf)
#ifdef VERBOSE
  print *, "-----------------"
  print *, int(V1h(   1,1:ey,1:ez))
  print *, int(V1h(  ex,1:ey,1:ez))
  print *, int(V1h(1:ex,   1,1:ez))
  print *, int(V1h(1:ex,  ey,1:ez))
  print *, int(V1h(1:ex,1:ey,   1))
  print *, int(V1h(1:ex,1:ey,  ez))
  print *, "-----------------"

  print *, "-----BB------"
  print *, int(BoundaryBuf( 0+1: 0+ey*ez))
  print *, int(BoundaryBuf( 9+1: 9+ey*ez))
  print *, int(BoundaryBuf(18+1:18+ex*ez))
  print *, int(BoundaryBuf(27+1:27+ex*ez))
  print *, int(BoundaryBuf(36+1:36+ex*ey))
  print *, int(BoundaryBuf(45+1:45+ex*ey))
  print *, "-----------------"
#endif
#endif

call stop(transfer_timer)

#ifdef DO_HALO_EXCHANGE
#ifdef VERBOSE
  print *, "EXCHANGING boundaries"
#endif
  ! exchange boundaries with neighbors (-1,N+1)
  CALL Exchange_Halo_3D(N,M,L,V1h,BoundaryBuf,RecvBuf)

! transfer_time = elapsed_time(cl_Relax_3D_%transfer_timer)
! transfer_time = elapsed_time(transfer_timer)
#endif
END DO 

! print *, " total gpu time    ==   ", real(total_gpu_time), " msec"
! print *, " average gpu time    ==   ", real(total_gpu_time)/nsteps, " msec (avg)"

print *, " average cpu time      ==   ", elapsed_time(cpu_timer) / nsteps
print *, " average gpu time      ==   ", (elapsed_time(gpu_timer)-elapsed_time(cpu_timer)) / nsteps
print *, " average transfer time ==   ", elapsed_time(transfer_timer) / nsteps

#undef DO_RELAX
#undef DO_GETBOUNDARY
#undef DO_HALO_EXCHANGE
#undef USE_MPI


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef DUMP_OUTPUT
cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V1h_,C_LOC(V1h),cl_size__)
WRITE(UNIT=fd,FMT=*) t, maxval(V1h)
CALL Textual_Output_3D(N,M,L,V1h,"1h_mid")
#endif

! print *, "||| PRE V1h"	
! print *, V1h(:,:,:)

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
  cl_gws__ = [N/2,M/2,L/2]

!cl_status__ = run(cl_Restrict_3D_,2,cl_gwo__,cl_gws__,cl_lws__)
cl_status__ = run(cl_Restrict_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
cl_status__ = clFinish(cl_Restrict_3D_%commands)
print *, "RESTRICT gws and lws"
print *, cl_gws__
print *, cl_lws__

#endif
#ifdef DUMP_OUTPUT
cl_size__ = 4*((N/2+1-(-1))+1)*((M/2+1-(-1))+1)*((L/2+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V2h_,C_LOC(V2h),cl_size__)
CALL Textual_Output_3D(N/2,M/2,L/2,V2h,"2h_0")
#endif

! print *, "===V2h"
! print *, V2h(:,:,:)
! print *, "===FIN"
#undef DO_RESTRICT
!#undef DO_PROLONGATE
#undef DO_RELAX
!#undef DUMP_OUTPUT

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

#ifdef DO_GETBOUNDARY
  ex = N/2-1
  ey = M/2-1
  ez = L/2-1

  !! put boundaries to device, indices (0,N/2) ...
  !
  call Copyto_Halo_Buf_3D(N/2, M/2, L/2, V2h, BoundaryBuf)

  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = writeBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  !! run copy memory kernel on device
  !
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,0,N/2)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,1,M/2)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,2,L/2)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,3,clMemObject(cl_V2h_))
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,4,clMemObject(cl_BoundaryBuf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = [5,5,5]
  cl_gws__ = [ey*ez + ex*ez + ex*ey,2,1]
  cl_status__ = run(cl_GetBoundary_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_GetBoundary_3D_%commands)

  !! get boundaries from device (1,N/2-1)
  !
  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = readBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  call Copyfrom_Halo_Buf_3D(N/2, M/2, L/2, V2h, BoundaryBuf)
#endif

#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(N/2,M/2,L/2,V2h,BoundaryBuf,RecvBuf)
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

#ifdef DO_GETBOUNDARY
  ex = N/4-1
  ey = M/4-1
  ez = L/4-1

  !! put boundaries to device, indices (0,N/4) ...
  !
  call Copyto_Halo_Buf_3D(N/4, M/4, L/4, V4h, BoundaryBuf)

  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = writeBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  !! run copy memory kernel on device
  !
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,0,N/4)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,1,M/4)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,2,L/4)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,3,clMemObject(cl_V4h_))
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,4,clMemObject(cl_BoundaryBuf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = [5,5,5]
  cl_gws__ = [ey*ez + ex*ez + ex*ey,2,1]
  cl_status__ = run(cl_GetBoundary_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_GetBoundary_3D_%commands)

  !! get boundaries from device (1,N/4-1)
  !
  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = readBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  call Copyfrom_Halo_Buf_3D(N/4, M/4, L/4, V4h, BoundaryBuf)
#endif

#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(N/4,M/4,L/4,V4h,BoundaryBuf,RecvBuf)
#endif
END DO 

#ifdef DUMP_OUTPUT
cl_size__ = 4*((N/4+1-(-1))+1)*((M/4+1-(-1))+1)*((L/4+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V4h_,C_LOC(V4h),cl_size__)
WRITE(UNIT=fd,FMT=*) t, maxval(V4h)
CALL Textual_Output_3D(N/4,M/4,L/4,V4h,"4h_mid")
#endif

!! This is a stub an actual solution at the finest grid level
!
V8h = 0

cl_size__ = 4*((N/8+1-(-1))+1)*((M/8+1-(-1))+1)*((L/8+1-(-1))+1)*1
cl_status__ = writeBuffer(cl_V8h_,C_LOC(V8h),cl_size__)

! #define DO_PROLONGATE
! #define DUMP_OUTPUT

cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
cl_status__ = writeBuffer(cl_V1h_,C_LOC(V1h),cl_size__)

#ifdef DO_PROLONGATE
focl_intvar__ = N
cl_status__ = setKernelArg(cl_Prolongate_3D_,0,focl_intvar__)
focl_intvar__ = M
cl_status__ = setKernelArg(cl_Prolongate_3D_,1,focl_intvar__)
focl_intvar__ = L
cl_status__ = setKernelArg(cl_Prolongate_3D_,2,focl_intvar__)
cl_status__ = setKernelArg(cl_Prolongate_3D_,3,clMemObject(cl_V1h_))
cl_status__ = setKernelArg(cl_Prolongate_3D_,4,clMemObject(cl_V2h_))
cl_gwo__ = [0,0,0]
cl_gws__ = [1,1,1]
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(1,cl_lws__,cl_gws__,[1,1,1])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/4+1-(-1))+1),((M/4+1-(-1))+1),((L/4+1-(-1))+1)])
cl_gws__ = focl_global_size(3,cl_lws__,cl_gws__,[((N/8+1-(-1))+1),((M/8+1-(-1))+1),((L/8+1-(-1))+1)])
cl_gws__ = [N,M,L]
cl_status__ = run(cl_Prolongate_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
cl_status__ = clFinish(cl_Prolongate_3D_%commands)
#endif

cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V1h_,C_LOC(V1h),cl_size__)
!WRITE(UNIT=fd,FMT=*) t, maxval(V1h)
! print *, "After PROLONGATE"
!print *, V1h
        
! print *, "After BoundaryBuf"
!print *, BoundaryBuf


#undef DO_PROLONGATE
!#undef DUMP_OUTPUT

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

#ifdef DO_GETBOUNDARY
  ex = N/4-1
  ey = M/4-1
  ez = L/4-1

  !! put boundaries to device, indices (0,N/4) ...
  !
  call Copyto_Halo_Buf_3D(N/4, M/4, L/4, V4h, BoundaryBuf)

  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = writeBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  !! run copy memory kernel on device
  !
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,0,N/4)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,1,M/4)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,2,L/4)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,3,clMemObject(cl_V4h_))
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,4,clMemObject(cl_BoundaryBuf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = [5,5,5]
  cl_gws__ = [ey*ez + ex*ez + ex*ey,2,1]
  cl_status__ = run(cl_GetBoundary_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_GetBoundary_3D_%commands)

  !! get boundaries from device (1,N/4-1)
  !
  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = readBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  call Copyfrom_Halo_Buf_3D(N/4, M/4, L/4, V4h, BoundaryBuf)
#endif

#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(N/4,M/4,L/4,V4h,BoundaryBuf,RecvBuf)
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

#ifdef DO_GETBOUNDARY
  ex = N/2-1
  ey = M/2-1
  ez = L/2-1

  !! put boundaries to device, indices (0,N/2) ...
  !
  call Copyto_Halo_Buf_3D(N/2, M/2, L/2, V2h, BoundaryBuf)

  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = writeBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  !! run copy memory kernel on device
  !
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,0,N/2)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,1,M/2)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,2,L/2)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,3,clMemObject(cl_V2h_))
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,4,clMemObject(cl_BoundaryBuf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = [5,5,5]
  cl_gws__ = [ey*ez + ex*ez + ex*ey,2,1]
  cl_status__ = run(cl_GetBoundary_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_GetBoundary_3D_%commands)

  !! get boundaries from device (1,N/2-1)
  !
  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = readBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  call Copyfrom_Halo_Buf_3D(N/2, M/2, L/2, V2h, BoundaryBuf)
#endif

#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(N/2,M/2,L/2,V2h,BoundaryBuf,RecvBuf)
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

#ifdef DO_GETBOUNDARY
  ex = N-1
  ey = M-1
  ez = L-1

  !! put boundaries to device, indices (0,N) ...
  !
  call Copyto_Halo_Buf_3D(N, M, L, V1h, BoundaryBuf)

  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = writeBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  !! run copy memory kernel on device
  !
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,0,N)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,1,M)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,2,L)
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,3,clMemObject(cl_V1h_))
  cl_status__ = setKernelArg(cl_GetBoundary_3D_,4,clMemObject(cl_BoundaryBuf_))
  cl_gwo__ = [0,0,0]
  cl_gws__ = [1,1,1]
  cl_gws__ = [5,5,5]
  cl_gws__ = [ey*ez + ex*ez + ex*ey,2,1]
  cl_status__ = run(cl_GetBoundary_3D_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_GetBoundary_3D_%commands)

  !! get boundaries from device (1,N-1)
  !
  cl_size__ = 4*(2*ey*ez + 2*ex*ez + 2*ex*ey)
  cl_status__ = readBuffer(cl_BoundaryBuf_,C_LOC(BoundaryBuf),cl_size__)

  call Copyfrom_Halo_Buf_3D(N, M, L, V1h, BoundaryBuf)
#endif

#ifdef DO_HALO_EXCHANGE
  CALL Exchange_Halo_3D(N,M,L,V1h,BoundaryBuf,RecvBuf)
#endif
END DO 

#ifdef DUMP_OUTPUT
cl_size__ = 4*((N+1-(-1))+1)*((M+1-(-1))+1)*((L+1-(-1))+1)*1
cl_status__ = readBuffer(cl_V1h_,C_LOC(V1h),cl_size__)
WRITE(UNIT=fd,FMT=*) t, maxval(V1h)
CALL Textual_Output_3D(N,M,L,V1h,"1h_end")
CLOSE(UNIT=fd)
#endif

#ifdef USE_MPI
call Parallel_End
#endif

END PROGRAM PoissonMultigrid
