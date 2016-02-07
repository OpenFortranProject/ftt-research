PROGRAM dijkstra_main
USE ForOpenCL
USE MPI_f08
USE forward_star
IMPLICIT NONE

character(len=3), parameter :: grid_name = '200'

!! WARNING: current must also change (NX,NY,NZ) in sweep.cl (maybe)
!
INTEGER :: nx = 200
INTEGER :: ny = 200
INTEGER :: nz = 200
INTEGER :: newNX = 224
INTEGER :: newNY = 224
INTEGER :: newNZ = 224

INTEGER, PARAMETER :: LWSX  = 224

INTEGER, PARAMETER :: DB   = 1
INTEGER, PARAMETER :: NPTS = 12
INTEGER, PARAMETER :: NFS  = 818

REAL, PARAMETER :: VERY_BIG = huge(1.0)/10.0

REAL,    ALLOCATABLE, TARGET, DIMENSION(:,:,:,:) :: TT
REAL,    ALLOCATABLE, TARGET, DIMENSION(:,:,:)   :: U
INTEGER, ALLOCATABLE, TARGET, DIMENSION(:,:,:)   :: Changed
INTEGER, ALLOCATABLE, TARGET, DIMENSION(:,:)     :: Offset
REAL,    ALLOCATABLE, TARGET, DIMENSION(:)       :: Dist

DOUBLE PRECISION :: time, time0, time_diff, time_sweep = 0.0d0, time_total = 0.0d0
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
INTEGER :: stepsTaken, totalSteps, change, pt, startPt(3,12)
REAL :: bandwidth  

ocl_id = 1
dev = get_subimage(ocl_id,cl_dev_)

!! May want information about the devices
cl_status__ = query(cl_dev_)
cl_sweep_ = createKernel(cl_dev_,"sweep_db")

print *, "   nx,    ny,    nz", nx, ny, nz
print *, "newNX, newNY, newNZ", newNX, newNY, newNZ

ALLOCATE(U(newNX,newNY,newNZ))
ALLOCATE(TT(newNX,newNY,newNZ,DB))
ALLOCATE(Changed(newNX,newNY,newNZ))
ALLOCATE(Offset(3,NFS), Dist(NFS))

cl_size__ = DB * 4*newNX*newNY*newNZ
cl_TT_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)

cl_size__ = 4*newNX*newNY*newNZ
cl_U_  = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
cl_Changed_ = createBuffer(cl_dev_,CL_MEM_READ_WRITE,cl_size__,C_NULL_PTR)
print *, "cl_U, cl_TT, cl_Changed size = ", cl_size__
cl_size__ = 4*3*NFS
cl_Offset_ = createBuffer(cl_dev_,CL_MEM_READ_ONLY,cl_size__,C_NULL_PTR)
cl_size__ = 4*NFS
cl_Dist_ = createBuffer(cl_dev_,CL_MEM_READ_ONLY,cl_size__,C_NULL_PTR)
print *, "cl_Offset = ", cl_size__*3, cl_size__

call read_starting_points(grid_name, NPTS, startPt)
call read_velocity_model_padded(grid_name, newNX,newNY,newNZ, nx,ny,nz, U)
call read_forward_star(NFS, Offset)
Changed(:,:,:) = 0

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
cl_status__ = setKernelArg(cl_sweep_,7,clMemObject(cl_Changed_))
cl_status__ = setKernelArg(cl_sweep_,8,stepsTaken)

cl_gwo__ = [0,0,0]
cl_lws__ = [LWSX, 1, 1]
cl_gws__ = [newNX,newNY,newNZ]

print *, "cl_lws__", cl_lws__
print *, "cl_gws__", cl_gws__

totalSteps = 0

time0 = MPI_Wtime()

do pt = 1, NPTS

  change = 0
  stepsTaken = 0

  !! initialize travel times
  !

  i = startPt(1,pt)
  j = startPt(2,pt)
  k = startPt(3,pt)
  TT = VERY_BIG
  TT(i,j,k,:) = 0.0

  print *
  print *, "starting point", pt, ':', i, j, k
  print *, "----------------------------------------------------------------"

  cl_size__ = DB * 4*newNX*newNY*newNZ
  cl_status__ = writeBuffer(cl_TT_,C_LOC(TT),cl_size__)

  done = .FALSE.
  do while (.not. done)

     time = MPI_Wtime()

     cl_status__ = run(cl_sweep_,3,cl_gwo__,cl_gws__,cl_lws__)
     cl_status__ = clFinish(cl_sweep_%commands)
     time_diff = time_sweep
     time_sweep = time_sweep + MPI_Wtime() - time
     time_diff = time_sweep - time_diff

     cl_size__ = 4*newNX*newNY*newNZ
     cl_status__ = readBuffer(cl_Changed_,C_LOC(Changed),cl_size__)
     IF (sum(Changed) .le. 0) done = .TRUE.
     cl_status__ = setKernelArg(cl_sweep_, 8, stepsTaken)
     stepsTaken = stepsTaken + 1

!!!     print *, "# changed:", sum(Changed), "step", stepsTaken, real(time_diff)
     if (done .eq. .TRUE. .and. change .lt. 1) then
        done = .FALSE.
        change = change + 1
     end if

  end do  
  totalSteps = totalSteps + stepsTaken
  print *, "# sweeps and cumulative sweep time", stepsTaken, real(time_sweep)
end do

time_total = MPI_Wtime() - time0

cl_size__   = 4*newNX*newNY*newNZ
cl_status__ = readBuffer(cl_TT_,C_LOC(TT),cl_size__)

print *, "------------------"
print *, U(i,j,1:10)
print *, "------------------"
print *, TT(i,j,1:10,1)
print *, "------------------"

PRINT *, ''
PRINT *, "Sweep/reduce time for N=", nx,ny,nz, real(time_total), real(time_sweep)
bandwidth = stepsTaken*4.0*nx*ny*nz*NFS*2.0 / (real(time_sweep) * 1000000000)
PRINT *, "Steps Taken", totalSteps, "Bandwidth", bandwidth

if (debug) then
  call write_results_padded(newNX,newNY,newNZ, nx,ny,nz, TT)
end if  

DEALLOCATE(U,TT,Changed,Offset,Dist)

cl_status__ = releaseMemObject(cl_U_)
cl_status__ = releaseMemObject(cl_TT_)
cl_status__ = releaseMemObject(cl_Changed_)
cl_status__ = releaseMemObject(cl_Offset_)
cl_status__ = releaseMemObject(cl_Dist_)

END PROGRAM
