PROGRAM dijkstra_main
USE MPI_f08
USE forward_star
IMPLICIT NONE

character(len=3), parameter :: grid_name = '250'

!! WARNING: current must also change (NX,NY,NZ) in sweep.cl (maybe)
!
INTEGER :: nx = 100
INTEGER :: ny = 100
INTEGER :: nz = 100
INTEGER :: newNX = 128
INTEGER :: newNY = 128
INTEGER :: newNZ = 128

INTEGER, PARAMETER :: LWSX  = 256

INTEGER, PARAMETER :: DB   = 1
INTEGER, PARAMETER :: NPTS = 1
INTEGER, PARAMETER :: NFS  = 818

REAL, PARAMETER :: VERY_BIG = huge(1.0)/10.0

REAL,    ALLOCATABLE, TARGET, DIMENSION(:,:,:,:) :: TT
REAL,    ALLOCATABLE, TARGET, DIMENSION(:,:,:)   :: U
INTEGER, ALLOCATABLE, TARGET, DIMENSION(:,:,:)   :: Changed
INTEGER, ALLOCATABLE, TARGET, DIMENSION(:,:)     :: Offset
REAL,    ALLOCATABLE, TARGET, DIMENSION(:)       :: Dist

DOUBLE PRECISION :: time, time0, time_diff, time_sweep = 0.0d0, time_total = 0.0d0
INTEGER :: i, j, k, ii, jj, kk
INTEGER :: l, is, js, ks, chg, times_changed = 0
REAL   :: distance, delay, u0, oi, oj, ok, t0, tt_min, t
LOGICAL :: done  = .FALSE.
LOGICAL :: debug = .TRUE.

INTEGER :: dev
INTEGER :: ocl_id

INTEGER :: stepsTaken, totalSteps, change, pt, startPt(3,12)
REAL :: bandwidth  


print *, "   nx,    ny,    nz", nx, ny, nz
print *, "newNX, newNY, newNZ", newNX, newNY, newNZ

ALLOCATE(U(newNX,newNY,newNZ))
ALLOCATE(TT(newNX,newNY,newNZ,DB))
ALLOCATE(Changed(newNX,newNY,newNZ))
ALLOCATE(Offset(3,NFS), Dist(NFS))

call read_starting_points(grid_name, NPTS, startPt)
call read_velocity_model_padded(grid_name, newNX,newNY,newNZ, nx,ny,nz, U)
call read_forward_star(NFS, Offset)
Changed(:,:,:) = 0

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

  k = 1
  done = .FALSE.
  do while (.not. done)
     
     time = MPI_Wtime()
     ! ===RUN KERNEL
     do i=1,NX
        do j=1,NY
           do k=1,NZ
              ! print *, "i=",i,"j=",j, "npts=",NPTS
              u0 = U(i,j,k)
              t0 = TT(i,j,k,DB) !PLUS TT_OFF ++NEED TO DO THIS
              tt_min = t0
              chg = 0
              do l=1, NFS
                 is = i + Offset(1,l);if ((is .le. 0) .OR. (is .ge. nx)) cycle
                 js = j + Offset(2,l);if ((js .le. 0) .OR. (js .ge. ny)) cycle
                 ks = k + Offset(3,l);if ((ks .le. 0) .OR. (ks .ge. nz)) cycle
                 oi = is - i;
                 oj = js - j;
                 ok = ks - k;
                 distance = 10.0*sqrt( oi*oi + oj*oj + ok*ok );
                 delay = 0.5*(u0 + U(is,js,ks)) * distance;
#ifdef UPDATE_FORWARD_STAR_TT
                 t = t0 + delay;
                 if (t .lt. TT(is, js, ks, DB)) then
                    chg = 1;
                    chg_star = 1;
                    TT(is,js,ks, DB) = t;
                 end if
#endif
                 t = TT(is,js,ks, DB) + delay
                 if (t .lt. t0) then
                    chg = 1
                    t0 = t
                    tt_min = t
                 end if
              end do
              Changed(i,j,k) = chg
              TT(i,j,k, DB) = tt_min ! PLUS OUT_TTOFF
           end do
        end do
     end do
!    if whatever then
!      kk += 1
!    end if


     time_diff = time_sweep
     time_sweep = time_sweep + MPI_Wtime() - time
     time_diff = time_sweep - time_diff
     print *,sum(Changed)
     IF (sum(Changed) .le. 0) then 
        times_changed = times_changed + 1
        IF (times_changed .eq. 2) then
           done = .TRUE.
        END IF
     END IF

     ! IF (sum(Changed) .le. 0) then
     !    k = k + 1
     !    if (k .eq. NZ) done = .TRUE.
     ! end IF
     stepsTaken = stepsTaken + 1

  end do  
  totalSteps = totalSteps + stepsTaken
  print *, "# sweeps and cumulative sweep time", stepsTaken, real(time_sweep)
end do

time_total = MPI_Wtime() - time0


print *, "------------------"
print *, U(i,j,1:10)
print *, "------------------"
print *, TT(i,j,1:10,1)
print *, "------------------"

PRINT *, ''
PRINT *, "total time, time sweep", real(time_total), real(time_sweep)
PRINT *, "nx ny nz", nx,ny,nz
bandwidth = stepsTaken*4.0*nx*ny*nz*NFS*2.0 / (real(time_sweep) * 1000000000)
PRINT *, "Steps Taken", totalSteps, "Bandwidth", bandwidth

if (debug) then
  call write_results_padded(newNX,newNY,newNZ, nx,ny,nz, TT)
end if  

DEALLOCATE(U,TT,Changed,Offset,Dist)

END PROGRAM
