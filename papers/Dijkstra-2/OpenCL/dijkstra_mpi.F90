PROGRAM dijkstra_main
USE ForOpenCL
USE MPI_f08
USE forward_star
IMPLICIT NONE

INTEGER :: NX = 256
INTEGER :: NY = 256
INTEGER :: NZ = 64
INTEGER :: PAD = 8
INTEGER :: newNX, newNY, newNZ
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
TYPE(CLBuffer) :: cl_TTBuf_
TYPE(CLBuffer) :: cl_Changed_
TYPE(CLBuffer) :: cl_Offset_
TYPE(CLKernel) :: cl_sweep_

INTEGER :: focl_intvar__
INTEGER(KIND=cl_int) :: cl_status__
INTEGER(KIND=c_size_t) :: cl_size__
INTEGER(KIND=c_size_t) :: cl_gwo__(3)
INTEGER(KIND=c_size_t) :: cl_gws__(3)
INTEGER(KIND=c_size_t) :: cl_lws__(3)
INTEGER :: stepsTaken = 0, rightHalo, change
REAL :: var, bandwidth  

ocl_id = 1
dev = get_subimage(ocl_id,cl_dev_)

!! May want information about the devices
cl_status__ = query(cl_dev_)
cl_sweep_ = createKernel(cl_dev_,"sweep_padded")

open(unit = 2, file = "velocity-241-241-51-nonConst.txt")
read (2,*), NX, NY, NZ
! NX = 4
! NY = 4
! NZ = 4

newNX = 256
newNY = 256
newNZ = NZ
rightHalo = newNX - NX
cl_lws__ = [8, 8, 1]
print *, "   NX,    NY,    NZ", NX, NY, NZ
print *, "newNX, newNY, newNZ", newNX, newNY, newNZ, "cl_lws__", cl_lws__

ALLOCATE(      U((1-PAD):newNX+PAD,(1-PAD):newNY+PAD,(1-PAD):newNZ+PAD))
ALLOCATE(     TT((1-PAD):newNX+PAD,(1-PAD):newNY+PAD,(1-PAD):newNZ+PAD))
ALLOCATE(Changed((1-PAD):newNX+PAD,(1-PAD):newNY+PAD,(1-PAD):newNZ+PAD))
ALLOCATE(Offset(3,NFS))
U = 0
Changed = 0
! Get array from file
DO k = 1, NZ
   DO j = 1, NY
      DO i = 1, NX
         read (2,*), var
         U(i,j,k) = var
      END DO
   END DO
END DO

! print *,U
cl_size__ = 4*(newNX+2*PAD)*(newNY+2*PAD)*(newNZ+2*PAD)
cl_U_  = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
cl_TT_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
cl_TTBuf_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
cl_Changed_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
print *, "cl_U, cl_TT, cl_Changed size = ", cl_size__
cl_size__ = 4*3*NFS
cl_Offset_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
print *, "cl_Offset = ", cl_size__

CALL read_forward_star(NFS,Offset)

TT = VERY_BIG

i = NX/2;  j = NY/2;  k = NZ/2
TT(i,j,k) = 0.0

cl_size__ = 4*(newNX+2*PAD)*(newNY+2*PAD)*(newNZ+2*PAD)
cl_status__ = writeBuffer(cl_U_, C_LOC(U ),cl_size__)
cl_status__ = writeBuffer(cl_TT_,C_LOC(TT),cl_size__)
cl_status__ = writeBuffer(cl_TTBuf_,C_LOC(TT),cl_size__)
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
cl_status__ = setKernelArg(cl_sweep_,8,rightHalo)
cl_status__ = setKernelArg(cl_sweep_,9,stepsTaken)
cl_status__ = setKernelArg(cl_sweep_,10,clMemObject(cl_TTBuf_))


change = 0
! DO WHILE(stepsTaken .lt. 6)
DO WHILE(.not. done)
  time = MPI_Wtime()

  cl_gwo__ = [0,0,0]
  cl_gws__ = [newNX,newNY,newNZ]

  cl_status__ = run(cl_sweep_,3,cl_gwo__,cl_gws__,cl_lws__)
  cl_status__ = clFinish(cl_sweep_%commands)
  time_sweep = time_sweep + MPI_Wtime() - time

  time = MPI_Wtime()
  cl_size__ = 4*NX*NY*NZ
  cl_status__ = readBuffer(cl_Changed_,C_LOC(Changed),cl_size__)
  IF (sum(Changed) .le. 0) done = .TRUE.
  cl_status__ = setKernelArg(cl_sweep_, 9,stepsTaken)
  stepsTaken = stepsTaken + 1
  time_reduce = time_reduce + MPI_Wtime() - time

  PRINT *, "# changed:", sum(Changed), "step", stepsTaken
  ! if (done .eq. .TRUE. .and. change .eq. 0) then
  !    done = .FALSE.
  !    change = 1
  ! end if
  ! done = .true.

END DO 

!!TODO - not multiple buffering anymore?

cl_size__ = 4*(newNX+2*PAD)*(newNY+2*PAD)*(newNZ+2*PAD)
if (mod(stepsTaken, 2) .eq. 0) then
   cl_status__ = readBuffer(cl_TT_,C_LOC(TT),cl_size__)
else
   cl_status__ = readBuffer(cl_TTBuf_,C_LOC(TT),cl_size__)
end if

open(unit = 7, file = "output.txt")

IF (debug) THEN
  PRINT *, ''
  DO k = 1, newNZ
     DO j = 1, newNY
        DO i = 1, newNX
           ! PRINT *, i,j,k,TT(i,j,k)
           ! write (7,*), i,j,k,TT(i,j,k)
           write (7,*), TT(i,j,k)
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
cl_status__ = releaseMemObject(cl_TTBuf_)
cl_status__ = releaseMemObject(cl_Changed_)
cl_status__ = releaseMemObject(cl_Offset_)

END PROGRAM
