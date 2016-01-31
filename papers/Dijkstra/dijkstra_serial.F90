program dijkstra_main
  use MPI_f08
  use forward_star
  use dijkstra
  implicit none

  !! dimensions for Joseph's example (101,161,51)
  !
  integer, parameter :: NX =  8
  integer, parameter :: NY =  8
  integer, parameter :: NZ =  1
  integer, parameter :: NFS = 818
  real,    parameter :: INFINITY = huge(1.0)
  real,    parameter :: VERY_BIG = huge(1.0)/10.0


  real,    allocatable ::       U(:,:,:)        ! slowness
  real,    allocatable ::      TT(:,:,:)        ! travel time
  integer, allocatable :: Changed(:,:,:)        ! 1 if tt updated at vertex, 0 otherwise
  integer, allocatable ::  Offset(:,:)          ! offset in forward star

  !  -----------------------------------------

  double precision :: time, time_sweep = 0.0d0, time_reduce = 0.0d0
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

  call read_forward_star(NFS, Offset)
#ifdef NO_NO_NO
  do i = 1, NFS
     print '(i4, i4, i4)', Offset(1,i), Offset(2,i), Offset(3,i)
  end do
#endif

  U = 1.0
!!!!  U(8:24,8:24,8:24) = 1.0  ! pick some "faster" regions

  TT = VERY_BIG

  !! sweep grid starting at (1,1,1)
  !
  i = NX;  j = NY;  k = NZ;
  TT(i,j,k) = 0.0

  i = 1
  do while (.NOT. done) 

     time = MPI_Wtime()
     call sweep(NX,NY,NZ, NFS, U, TT, Offset, Changed)
     time_sweep = time_sweep + MPI_Wtime() - time

     !! see if any travel times have changed
     !
     time = MPI_Wtime()
     if (sum(Changed) == 0) done = .TRUE.
     time_reduce = time_reduce + MPI_Wtime() - time

     print *, "# changed:", sum(Changed)

  end do

#ifdef NO_NO_NO
#endif
  if (debug) then
     print *
     do k = 1, NZ
        do j = 1, NY
           do i = 1, NX
              print *, i, j, k, TT(i,j,k)
           end do
        end do
     end do
  end if

  print *
  print *, "Sweep/reduce time for N=", NX*NY*NZ, real(time_sweep), real(time_reduce)

  deallocate(U,TT,Changed,Offset)

CONTAINS

  integer function get_subimage(ocl_id)
    integer, intent(in) :: ocl_id
    get_subimage = -1
  end function

end program
