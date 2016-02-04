PROGRAM dijkstra_main
USE ForOpenCL
USE MPI_f08
USE forward_star
IMPLICIT NONE

!! WARNING: current must also change (NX,NY,NZ) in sweep.cl
!
INTEGER :: nx = 256
INTEGER :: ny = 256
INTEGER :: nz = 64
INTEGER :: DB = 2
INTEGER :: newNX, newNY, newNZ
INTEGER, PARAMETER :: NFS = 818
REAL, PARAMETER :: VERY_BIG = huge(1.0)/10.0

REAL,    ALLOCATABLE, TARGET, DIMENSION(:,:,:,:) :: TT
REAL,    ALLOCATABLE, TARGET, DIMENSION(:,:,:)   :: U
INTEGER, ALLOCATABLE, TARGET, DIMENSION(:,:,:)   :: Changed
INTEGER, ALLOCATABLE, TARGET, DIMENSION(:,:)     :: Offset
INTEGER, ALLOCATABLE, TARGET, DIMENSION(:)       :: Dist

DOUBLE PRECISION :: time, time_diff, time_sweep = 0.0d0, time_reduce = 0.0d0
INTEGER :: i, j, k
LOGICAL :: done = .FALSE.
LOGICAL :: debug = .TRUE.

INTEGER :: dev
INTEGER :: ocl_id

TYPE(CLDevice) :: cl_dev_
TYPE(CLBuffer) :: cl_U_
TYPE(CLBuffer) :: cl_TT_
TYPE(CLBuffer) :: cl_Changed_
TYPE(CLBuffer) :: cl_Offset_
TYPE(CLBuffer) :: cl_Dist_
TYPE(CLKernel) :: cl_sweep_

INTEGER :: focl_intvar__
INTEGER(KIND=cl_int) :: cl_status__
INTEGER(KIND=c_size_t) :: cl_size__
INTEGER(KIND=c_size_t) :: cl_gwo__(3)
INTEGER(KIND=c_size_t) :: cl_gws__(3)
INTEGER(KIND=c_size_t) :: cl_lws__(3)
INTEGER :: stepsTaken, rightHalo, change
REAL :: bandwidth  

ocl_id = 1
dev = get_subimage(ocl_id,cl_dev_)

!! May want information about the devices
cl_status__ = query(cl_dev_)
cl_sweep_ = createKernel(cl_dev_,"sweep_db")

open(unit = 2, file = "velocity-241-241-51-nonConst.txt")
read (2,*), nx, ny, nz

newNX = 256
newNY = 256
newNZ = 64
rightHalo = newNX - nx     ! WARNING: assert (newNX == newNY) as only one rightHalo used
print *, "   nx,    ny,    nz", nx, ny, nz
print *, "newNX, newNY, newNZ", newNX, newNY, newNZ, "cl_lws__", cl_lws__

ALLOCATE(U(newNX,newNY,newNZ))
ALLOCATE(TT(newNX,newNY,newNZ,DB))
ALLOCATE(Changed(newNX,newNY,newNZ))
ALLOCATE(Offset(3,NFS), Dist(NFS))

U(:,:,:) = 0
Changed(:,:,:) = 0

! Get array from file
DO k = 1, nz
   DO j = 1, ny
      DO i = 1, nx
         read (2,*), U(i,j,k)
      END DO
   END DO
END DO
close(2)

cl_size__ = DB * 4*newNX*newNY*newNZ
cl_TT_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)

cl_size__ = 4*newNX*newNY*newNZ
cl_U_  = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
cl_Changed_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
print *, "cl_U, cl_TT, cl_Changed size = ", cl_size__
cl_size__ = 4*3*NFS
cl_Offset_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
cl_size__ = 4*NFS
cl_Dist_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
print *, "cl_Offset = ", cl_size__*3, cl_size__

CALL read_forward_star(NFS,Offset)

TT = VERY_BIG

i = nx/2;  j = ny/2;  k = nz/2
i =  1;  j =  1;  k =  1
TT(i,j,k,:) = 0.0

cl_size__ = DB * 4*newNX*newNY*newNZ
cl_status__ = writeBuffer(cl_TT_,C_LOC(TT),cl_size__)
cl_size__ = 4*newNX*newNY*newNZ
cl_status__ = writeBuffer(cl_U_, C_LOC(U ),cl_size__)
cl_status__ = writeBuffer(cl_Changed_,C_LOC(Changed),cl_size__)
cl_size__ = 4*3*NFS
cl_status__ = writeBuffer(cl_Offset_,C_LOC(Offset),cl_size__)
cl_size__ = 4*NFS
cl_status__ = writeBuffer(cl_Dist_,  C_LOC(Dist  ),cl_size__)

cl_status__ = setKernelArg(cl_sweep_,0,nx)
cl_status__ = setKernelArg(cl_sweep_,1,ny)
cl_status__ = setKernelArg(cl_sweep_,2,nz)
cl_status__ = setKernelArg(cl_sweep_,3,NFS)
cl_status__ = setKernelArg(cl_sweep_,4,clMemObject(cl_U_))
cl_status__ = setKernelArg(cl_sweep_,5,clMemObject(cl_TT_))
cl_status__ = setKernelArg(cl_sweep_,6,clMemObject(cl_Offset_))
!cl_status__ = setKernelArg(cl_sweep_,7,clMemObject(cl_Dist_))
cl_status__ = setKernelArg(cl_sweep_,7,clMemObject(cl_Changed_))
cl_status__ = setKernelArg(cl_sweep_,8,rightHalo)
cl_status__ = setKernelArg(cl_sweep_,9,stepsTaken)

change = 0
stepsTaken = 0

! DO WHILE(stepsTaken .lt. 6)
DO WHILE(.not. done)

  time = MPI_Wtime()

  cl_gwo__ = [0,0,0]
  cl_lws__ = [64, 1, 1]
  cl_gws__ = [newNX,newNY,newNZ]

  cl_status__ = run(cl_sweep_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_sweep_%commands)
  time_diff = time_sweep
  time_sweep = time_sweep + MPI_Wtime() - time
  time_diff = time_sweep - time_diff

  time = MPI_Wtime()
  cl_size__ = 4*nx*ny*nz
  cl_status__ = readBuffer(cl_Changed_,C_LOC(Changed),cl_size__)
  IF (sum(Changed) .le. 0) done = .TRUE.
  cl_status__ = setKernelArg(cl_sweep_, 9,stepsTaken)
  stepsTaken = stepsTaken + 1
  time_reduce = time_reduce + MPI_Wtime() - time

  PRINT *, "# changed:", sum(Changed), "step", stepsTaken, time_diff
  if (done .eq. .TRUE. .and. change .lt. 3) then
     done = .FALSE.
     change = change + 1
  end if

END DO 

cl_size__ = DB * 4*newNX*newNY*newNZ
cl_status__ = readBuffer(cl_TT_,C_LOC(TT),cl_size__)

open(unit = 7, file = "output.txt")

IF (debug) THEN
  write (7,*), nx, ny, nz
  DO k = 1, nz
     DO j = 1, ny
        DO i = 1, nx
           write (7,*), i,j,k, TT(i,j,k,1)
        END DO
     END DO
  END DO 
END IF
close(7)

PRINT *, ''
PRINT *, "Sweep/reduce time for N=", nx*ny*nz, real(time_sweep), real(time_reduce)
bandwidth = stepsTaken*4.0*nx*ny*nz*NFS*2.0 / (real(time_sweep) * 1000000000)
PRINT *, "Steps Taken", stepsTaken, "Bandwidth", bandwidth

DEALLOCATE(U,TT,Changed,Offset,Dist)

cl_status__ = releaseMemObject(cl_U_)
cl_status__ = releaseMemObject(cl_TT_)
cl_status__ = releaseMemObject(cl_Changed_)
cl_status__ = releaseMemObject(cl_Offset_)
cl_status__ = releaseMemObject(cl_Dist_)

END PROGRAM
