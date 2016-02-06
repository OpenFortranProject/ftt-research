program dijkstra_main
  use MPI_f08
  use forward_star
  use dijkstra
  implicit none

  !! dimensions for Joseph's example (101,161,51)
  !
  integer, parameter :: NX  =  256                   ! size in x
  integer, parameter :: NY  =  256                   ! size in y
  integer, parameter :: NZ  =   64                   ! size in z
  integer, parameter :: DB  =    2                   ! double buffer dimension
  integer, parameter :: NFS =  818                   ! number in forward star
  integer, parameter :: NDEV =   2                   ! number of devices
  integer, parameter :: MAX_ST = 11                  ! maximum number of starting points
  real,    parameter :: VERY_BIG = huge(1.0)/10.0    ! start value for travel time

  integer :: padNX, padNY, padNZ                     ! padded sizes
  integer :: nStart                                  ! number of starting points

  real,    allocatable, target, dimension(:,:,:,:,:), codimension[:] :: TT      ! travel time
  real,    allocatable, target, dimension(:,:,:    ), codimension[:] :: U       ! slowness
  integer, allocatable, target, dimension(:,:,:    ), codimension[:] :: Changed ! 1 if tt updated
  integer, allocatable, target, dimension(:,:      ), codimension[:] :: Offset  ! offset forward star

  !  -----------------------------------------

  double precision :: time, time_diff, time_sweep = 0.0d0, time_total = 0.0d0
  integer :: i, j, k, istart
  logical :: done(NDEV)
  logical :: debug = .FALSE.

  integer :: start(3,MAX_ST)       ! starting points
  integer :: dev[NDEV]             ! CAFe subimage devices
  integer :: id                    ! OpenCL device id
  integer :: stepsTaken, rightHalo, change
  real    :: bandwidth  
  
  do id = 1, NDEV
     dev[id] = get_subimage(id)
  end do

  !! allocate space on this image and subimages
  !
  padNX = 256
  padNY = 256
  padNZ =  64

  allocate(     TT(padNX,padNY,padNZ,NB,NDEV) [*])
  allocate(      U(padNX,padNY,padNZ)         [*])
  allocate(Changed(padNX,padNY,padNZ)         [*])
  allocate( Offset(3,NFS)                     [*])

  do id = 1, NDEV
     allocate(     TT(padNX,padNY,padNZ,NB,1) [*])  [[dev(id)]]
     allocate(      U(padNX,padNY,padNZ)      [*])  [[dev(id)]]
     allocate(Changed(padNX,padNY,padNZ)      [*])  [[dev(id)]]
     allocate( Offset(3,NFS)                  [*])  [[dev(id)]]
  end do

  call read_forward_star(NFS, Offset)
  call read_velocity_model(NX, NY, NZ, U)
  call read_starting_points(MAX_ST, nStart, start)

  !! copy constant values to the devices
  !
  do id = 1, NDEV
     U [id] = U
     Offset [id] = Offset
  end do

  !! Loop over starting points
  !
  time = MPI_Wtime()
  do istart = 1, nStart, NDEV

     done(:) = .FALSE.

     do id = 1, NDEV
        !! sweep grid from starting point
        !
        i = start(1,istart+id-1)
        j = start(2,istart+id-1)
        k = start(3,istart+id-1)
        TT(:,:,:,id) = VERY_BIG
        TT(i,j,k,id) = 0.0
        TT(:,:,:,1)[id] = TT(:,:,:,id)

        nSweeps(id) = 0
        do while (.NOT. done) 

           call sweep(NX,NY,NZ, NFS, U[dev], TT[dev], Offset[dev], Changed[dev])  [[id, WITH_EVENT=evt(id)]]

           !! see if any travel times have changed
           !
           Changed = Changed[id]
           if (sum(Changed) == 0) done[id] = .TRUE.
           nSweeps(id) = nSweeps(id) + 1

           print *, id, "# changed:", sum(Changed)

        end do
     end do
  end do

  time_total = MPI_Wtime() - time

  print *
  print *, "Sweep time for N=", NX*NY*NZ, real(time_total)

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

  deallocate(U,TT,Changed,Offset)

  do id = 1, NDEV
     deallocate(      U)  [[dev(id)]]
     deallocate(     TT)  [[dev(id)]]
     deallocate(Changed)  [[dev(id)]]
     deallocate( Offset)  [[dev(id)]]
  end do

end program
