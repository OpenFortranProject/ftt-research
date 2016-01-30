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

  real,    allocatable ::       U(:,:,:)        ! slowness
  real,    allocatable ::      TT(:,:,:)        ! travel time
  integer, allocatable :: Changed(:,:,:)        ! 1 if tt updated in cell, 0 otherwise
  integer, allocatable ::  Offset(:,:)          ! offset in forward star

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
  allocate(      U(NX,NY,NZ))
  allocate(     TT(NX,NY,NZ))
  allocate(Changed(NX,NY,NZ))
  allocate( Offset(3,NFS)   )

  U = 3.0
  U(8:24,8:24,8:24) = 1.0  ! pick some "faster" regions

  TT = INFINITY

  !! relax grid starting at (1,1,1)
  !
  i = 1;  j = 1;  k = 1;
  U(i,j,k) = 0.0

  do while (.NOT. done) 

     time = MPI_Wtime()
     call relax(NX,NY,NZ, NFS, U, TT, Offset, Changed)
     time_relax = time_relax + MPI_Wtime() - time

     !! see if any travel times have changed
     !
     time = MPI_Wtime()
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

CONTAINS

  integer function get_subimage(ocl_id)
    integer, intent(in) :: ocl_id
    get_subimage = -1
  end function


end program
