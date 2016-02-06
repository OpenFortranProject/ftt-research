program dijkstra_main
  use MPI_f08
  use forward_star
  use dijkstra
  implicit none

  !! dimensions for Joseph's example (101,161,51)
  !
  integer, parameter :: NX =  241
  integer, parameter :: NY =  241
  integer, parameter :: NZ =  51
  integer, parameter :: NFS = 818
  real,    parameter :: INFINITY = huge(1.0)
  real,    parameter :: VERY_BIG = huge(1.0)/10.0

  real,    allocatable ::       U(:,:,:)        ! slowness
  real,    allocatable ::      TT(:,:,:)        ! travel time
  integer, allocatable :: Changed(:,:,:)        ! 1 if tt updated at vertex, 0 otherwise
  integer, allocatable ::  Offset(:,:)          ! offset in forward star

  !  -----------------------------------------

  double precision :: time, time0, time_sweep = 0.0d0, time_total = 0.0d0
  integer :: i, j, k, num_changed
  logical :: done  = .FALSE.
  logical :: debug = .FALSE.

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

  call read_forward_star(NFS, Offset)
print *, "---------------------------"
  call read_velocity_model(NX, NY, NZ, U)
print *, "..........................."

  Changed = 0
  TT = VERY_BIG

  !! sweep grid starting at midpoint (NX/2,NY/2,1) (roughly)
  !
  i = 1+NX/2;  j = 1+NY/2;  k = 1;
  TT(i,j,k) = 0.0

  time0 = MPI_Wtime()
  do while (.NOT. done) 

     time = MPI_Wtime()
     call sweep(NX,NY,NZ, NFS, U, TT, Offset, Changed)
     time = MPI_Wtime() - time
     time_sweep = time_sweep + time

     !! see if any travel times have changed
     !
     num_changed = sum(Changed)
     if (num_changed == 0) done = .TRUE.
     print *, "# changed:", num_changed, time

  end do

  time_total = MPI_Wtime() - time0

  if (debug) then
     call write_results(NX, NY, NZ, TT)
  end if

  print *
  print *, "Sweep/reduce time for N=", NX*NY*NZ, real(time_total), real(time_sweep)

  deallocate(U,TT,Changed,Offset)

CONTAINS

  integer function get_subimage(ocl_id)
    integer, intent(in) :: ocl_id
    get_subimage = -1
  end function

end program
