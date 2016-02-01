PROGRAM dijkstra_main
USE ForOpenCL
USE MPI_f08
USE forward_star
IMPLICIT NONE

INTEGER, PARAMETER :: NX = 128
INTEGER, PARAMETER :: NY = 128
INTEGER, PARAMETER :: NZ = 32
INTEGER, PARAMETER :: NFS = 818
REAL, PARAMETER :: VERY_BIG = huge(1.0)/10.0

REAL,    ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: U
REAL,    ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: TT
INTEGER, ALLOCATABLE, TARGET, DIMENSION(:,:,:) :: Changed
INTEGER, ALLOCATABLE, TARGET, DIMENSION(:,:)   :: Offset

DOUBLE PRECISION :: time, time_sweep = 0.0d0, time_reduce = 0.0d0
INTEGER :: i, j, k
LOGICAL :: done = .FALSE.
! LOGICAL :: debug = .TRUE.
LOGICAL :: debug = .FALSE.

INTEGER :: dev
INTEGER :: ocl_id

TYPE(CLDevice) :: cl_dev_
TYPE(CLBuffer) :: cl_U_
TYPE(CLBuffer) :: cl_TT_
TYPE(CLBuffer) :: cl_Changed_
TYPE(CLBuffer) :: cl_Offset_
TYPE(CLKernel) :: cl_sweep_

INTEGER :: focl_intvar__
INTEGER(KIND=cl_int) :: cl_status__
INTEGER(KIND=c_size_t) :: cl_size__
INTEGER(KIND=c_size_t) :: cl_gwo__(3)
INTEGER(KIND=c_size_t) :: cl_gws__(3)
INTEGER(KIND=c_size_t) :: cl_lws__(3) = [128,1,1]
INTEGER :: stepsTaken = 0
REAL    :: bandwidth  

ocl_id = 1
dev = get_subimage(ocl_id,cl_dev_)

!! May want information about the devices
cl_status__ = query(cl_dev_)

cl_sweep_ = createKernel(cl_dev_,"sweep")

ALLOCATE(U(NX,NY,NZ))
ALLOCATE(TT(NX,NY,NZ))
ALLOCATE(Changed(NX,NY,NZ))
ALLOCATE(Offset(3,NFS))

cl_size__ = 4*NX*NY*NZ
cl_U_  = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
cl_TT_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
cl_Changed_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
cl_size__ = 4*3*NFS
cl_Offset_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)

CALL read_forward_star(NFS,Offset)

U = 1.0
TT = VERY_BIG

i = NX/2;  j = NY/2;  k = NZ/2
TT(i,j,k) = 0.0

cl_size__ = 4*NX*NY*NZ
cl_status__ = writeBuffer(cl_U_, C_LOC(U ),cl_size__)
cl_status__ = writeBuffer(cl_TT_,C_LOC(TT),cl_size__)
cl_status__ = writeBuffer(cl_Changed_,C_LOC(Changed),cl_size__)
cl_size__ = 4*3*NFS
cl_status__ = writeBuffer(cl_Offset_,C_LOC(Offset),cl_size__)

cl_status__ = setKernelArg(cl_sweep_,0,NX)
cl_status__ = setKernelArg(cl_sweep_,1,NY)
cl_status__ = setKernelArg(cl_sweep_,2,NZ)
cl_status__ = setKernelArg(cl_sweep_,3,NFS)
cl_status__ = setKernelArg(cl_sweep_,4,clMemObject(cl_U_))
cl_status__ = setKernelArg(cl_sweep_,5,clMemObject(cl_TT_))
cl_status__ = setKernelArg(cl_sweep_,6,clMemObject(cl_Offset_))
cl_status__ = setKernelArg(cl_sweep_,7,clMemObject(cl_Changed_))

#ifdef NOT_YET
#endif
!stop "NOT_FINISHED"


DO WHILE(.NOT. done)
  time = MPI_Wtime()

  cl_gwo__ = [0,0,0]
  cl_gws__ = [NX,NY,NZ]

  cl_status__ = run(cl_sweep_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_sweep_%commands)
  time_sweep = time_sweep + MPI_Wtime() - time

  time = MPI_Wtime()
  cl_size__ = 4*NX*NY*NZ
  cl_status__ = readBuffer(cl_Changed_,C_LOC(Changed),cl_size__)
  IF (sum(Changed) == 0) done = .TRUE.
  time_reduce = time_reduce + MPI_Wtime() - time

  stepsTaken = stepsTaken + 1
  ! PRINT *, "# changed:", sum(Changed), "step", stepsTaken

  ! done = .true.

END DO 

cl_size__ = 4*NX*NY*NZ
cl_status__ = readBuffer(cl_TT_,C_LOC(TT),cl_size__)

open(unit = 7, file = "output.txt")

IF (debug) THEN
  PRINT *, ''
  DO k = 1, NZ
     DO j = 1, NY
        DO i = 1, NX
           PRINT *, i,j,k,TT(i,j,k)
           ! write (7,*), i,j,k,TT(i,j,k)
        END DO
     END DO
  END DO 
END IF
close(7)

PRINT *, ''
PRINT *, "Sweep/reduce time for N=", NX*NY*NZ, real(time_sweep), real(time_reduce)
bandwidth = stepsTaken*4.0*NX*NY*NZ*NFS*2.0 / (real(time_sweep) * 1000000000)
PRINT *, "Steps Taken", stepsTaken, "Bandwidth", bandwidth

DEALLOCATE(U,TT,Changed,Offset)

cl_status__ = releaseMemObject(cl_U_)
cl_status__ = releaseMemObject(cl_TT_)
cl_status__ = releaseMemObject(cl_Changed_)
cl_status__ = releaseMemObject(cl_Offset_)

END PROGRAM
