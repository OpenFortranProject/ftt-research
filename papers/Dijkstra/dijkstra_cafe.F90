program dijkstra_main
  use MPI_f08
  implicit none

  !! dimensions for Joseph's example (101,161,51)
  !
  integer, parameter :: NX = 32
  integer, parameter :: NY = 32
  integer, parameter :: NZ = 32
  integer, parameter :: NFS = 818
  real,    parameter :: INFINITY = huge(1.0)

#ifdef HAVE_COARRAYS
  real,    allocatable ::       U(:,:,:)[:]     ! slowness
  real,    allocatable ::      TT(:,:,:)[:]     ! travel time
  integer, allocatable :: Changed(:,:,:)[:]     ! 1 if tt updated in cell, 0 otherwise
  integer, allocatable ::  Offset(:,:)  [:]     ! offset in forward star
#else
  real,    allocatable ::       U(:,:,:)        ! slowness
  real,    allocatable ::      TT(:,:,:)        ! travel time
  integer, allocatable :: Changed(:,:,:)        ! 1 if tt updated in cell, 0 otherwise
  integer, allocatable ::  Offset(:,:)          ! offset in forward star
#endif

  !  -----------------------------------------

  double precision :: time, time_relax = 0.0d0, time_min = 0.0d0
  integer :: i, j, k
  logical :: done  = .FALSE.
  logical :: debug = .TRUE.

  integer :: dev                   ! CAFe subimage device
  integer :: ocl_id                ! OpenCL device id
  
  ocl_id = 1
  dev = get_subimage(ocl_id)

  !! allocate space on this image
  !
#ifdef HAVE_COARRAYS
  allocate(      U(NX,NY,NZ) [*])
  allocate(     TT(NX,NY,NZ) [*])
  allocate(Changed(NX,NY,NZ) [*])
  allocate( Offset(3,NFS)    [*])
#else
  allocate(      U(NX,NY,NZ))
  allocate(     TT(NX,NY,NZ))
  allocate(Changed(NX,NY,NZ))
  allocate( Offset(3,NFS)   )
#endif

#ifdef HAVE_DEVICE
  if (dev /= THIS_IMAGE()) then
     allocate(      V(NX,NY,NZ) [*])  [[dev]]
     allocate(     TT(NX,NY,NZ) [*])  [[dev]]
     allocate(Changed(NX,NY,NZ) [*])  [[dev]]
     allocate( Offset(3,NFS)    [*])  [[dev]]
  end if
#endif

  U  = 3.0
  U(8:24,8:24,8:24) = 1.0  ! pick some "slower" regions

  TT = INFINITY

  !! relax grid starting at (1,1,1)
  !
  i = 1;  j = 1;  k = 1;
  U(i,j,k) = 0.0

  !! copy initial values to the device
  !
#ifdef HAVE_COARRAYS
  U [dev] = U
  TT[dev] = TT
  Offset[dev] = Offset
#endif

  do while (.NOT. done) 

     time = MPI_Wtime()
#ifdef HAVE_COARRAYS
     call relax(NX,NY,NZ, NFS, U[dev], TT[dev], Offset[dev], Changed[dev])  [[dev]]
#else
     call relax(NX,NY,NZ, NFS, U     , TT     , Offset     , Changed     )
#endif
     time_relax = time_relax + MPI_Wtime() - time

     !! see if any travel times have changed
     !
     time = MPI_Wtime()
#ifdef HAVE_COARRAYS
     Changed = Changed[dev]
#endif
     if (sum(Changed) == 0) done = .TRUE.
     time_min = time_min + MPI_Wtime() - time

  end do

  if (debug) then
     print *
     do i = 1, NX
        do j = 1, NY
           do k = 1, NZ
              print *, i, j, k, TT(i,j,k)
           end do
        end do
     end do
  end if

  print *
  print *, "Relaxation/min time for N=", NX*NY*NZ, real(time_relax), real(time_min)

  deallocate(U,TT,Changed,Offset)

#if HAVE_DEVICE
  if (dev /= THIS_IMAGE()) then
     deallocate(      V)  [[dev]]
     deallocate(     TT)  [[dev]]
     deallocate(Changed)  [[dev]]
     deallocate( Offset)  [[dev]]
  end if
#endif

CONTAINS

  integer function get_subimage(ocl_id)
    integer, intent(in) :: ocl_id
    get_subimage = -1
  end function


end program
