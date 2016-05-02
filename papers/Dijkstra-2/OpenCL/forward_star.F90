module forward_star
  use MPI_F08

  real, parameter :: DIST_SCALE = 10.0   ! grid size of 10 km

contains

subroutine read_starting_points(grid_name, npts, start)
  implicit none
  character(len=3), intent(in) :: grid_name
  integer, intent(out) :: npts
  integer, intent(out), allocatable :: start(:,:)
  integer :: i, rank

  call MPI_Comm_rank(MPI_COMM_WORLD, rank)

  if (rank == 0) then
     open(unit=1, file='starting-points-' // grid_name // '.txt')

     read(1, *) npts
     allocate(start(3,npts))
     do i = 1, npts
        read(1, *) start(1,i), start(2,i), start(3,i)
     end do
     close(unit=1)

     call MPI_Bcast(npts, 1, MPI_INTEGER, 0, MPI_COMM_WORLD)
  else
     call MPI_Bcast(npts, 1, MPI_INTEGER, 0, MPI_COMM_WORLD)
     allocate(start(3,npts))
  end if

  call MPI_Bcast(start, 3*npts, MPI_INTEGER, 0, MPI_COMM_WORLD)

end subroutine read_starting_points

subroutine read_forward_star(nfs, off)
  implicit none
  integer, intent(in ) :: nfs
  integer, intent(out) :: off(3,nfs)
  integer :: i, npts, rank

  call MPI_Comm_rank(MPI_COMM_WORLD, rank)

  if (rank == 0) then
     ! open(unit=1, file='arcList.txt')
     open(unit=1, file='818-FS.txt')

     read(1, *) npts
     if (npts /= nfs) then
        stop "read_forward_star: # points don't match"
     end if

     do i = 1, nfs
        read(1, *) off(1,i), off(2,i), off(3,i)
     end do
     close(unit=1)
  end if

  call MPI_Bcast(off, 3*nfs, MPI_INTEGER, 0, MPI_COMM_WORLD)

end subroutine read_forward_star

subroutine read_velocity_model(grid_name, nxt,nyt,nzt, nx,ny,nz, U)
  implicit none
  character(len=3), intent(in) :: grid_name
  integer, intent(in ) :: nxt, nyt, nzt, nx, ny, nz
  real,    intent(out) :: U(nxt,nyt,nzt)
  integer :: i, j, k, oi, oj, ok, rank

  call MPI_Comm_rank(MPI_COMM_WORLD, rank)

  if (rank == 0) then
     open(unit=1, file='velocity-' // grid_name // '.txt')
     do i = 1, nx
        do j = 1, ny
           do k = 1, nz
              read(1, *) oi, oj, ok, U(i,j,k)
           end do
        end do
     end do
     close(unit=1)
  end if

  call MPI_Bcast(U, nxt*nyt*nzt, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD)

end subroutine read_velocity_model

subroutine calc_distance(nfs, off, dist)
  implicit none
  integer, intent(in ) :: nfs
  integer, intent(in ) :: off(3,nfs)
  real,    intent(out) :: dist(nfs)
  integer :: i, oi, oj, ok

  do i = 1, nfs
     oi = off(1,i)
     oj = off(2,i)
     ok = off(3,i)
     dist(i) = DIST_SCALE*sqrt( REAL(oi*oi + oj*oj + ok*ok) );
  end do

end subroutine calc_distance

subroutine calc_linear_paths(nx, ny, nz, nfs, U, start, tt)
  implicit none
  integer, intent(in ) :: nx, ny, nz, nfs, start(3)
  real,    intent(in ) ::  U(nx,ny,nz)
  real,    intent(out) :: tt(nx,ny,nz)

  real    :: delay
  integer :: i, j, k, si, sj, sk

  si = start(1)
  sj = start(2)
  sk = start(3)

  !! traverse x axis
  !
  do i = si-1, 1, -1
     delay = 0.5 * (U(i+1,sj,sk) + U(i,sj,sk)) * DIST_SCALE;
     tt(i,sj,sk) = tt(i+1,sj,sk) + delay
  end do
  do i = si+1, nx
     delay = 0.5 * (U(i-1,sj,sk) + U(i,sj,sk)) * DIST_SCALE;
     tt(i,sj,sk) = tt(i-1,sj,sk) + delay
  end do

  !! traverse y axis
  !
  do j = sj-1, 1, -1
     delay = 0.5 * (U(si,j+1,sk) + U(si,j,sk)) * DIST_SCALE;
     tt(si,j,sk) = tt(si,j+1,sk) + delay
  end do
  do j = sj+1, ny
     delay = 0.5 * (U(si,j-1,sk) + U(si,j,sk)) * DIST_SCALE;
     tt(si,j,sk) = tt(si,j-1,sk) + delay
  end do

  !! traverse x axis
  !
  do k = sk-1, 1, -1
     delay = 0.5 * (U(si,sj,k+1) + U(si,sj,k)) * DIST_SCALE;
     tt(si,sj,k) = tt(si,sj,k+1) + delay
  end do
  do k = sk+1, nz
     delay = 0.5 * (U(si,sj,k-1) + U(si,sj,k)) * DIST_SCALE;
     tt(si,sj,k) = tt(si,sj,k-1) + delay
  end do

end subroutine calc_linear_paths

subroutine calc_linear_paths_padded(padNx, padNy, padNz, nx, ny, nz, nfs, U, start, tt)
  implicit none
  integer, intent(in) :: padNx, padNy, padNz, nx, ny, nz, nfs, start(3)
  real,    intent(in ) ::  U(padNx,padNy,padNz)
  real,    intent(out) :: tt(padNx,padNy,padNz)

  real    :: delay
  integer :: i, j, k, si, sj, sk

  si = start(1)
  sj = start(2)
  sk = start(3)

  !! traverse x axis
  !
  do i = si-1, 1, -1
     delay = 0.5 * (U(i+1,sj,sk) + U(i,sj,sk)) * DIST_SCALE;
     tt(i,sj,sk) = tt(i+1,sj,sk) + delay
  end do
  do i = si+1, nx
     delay = 0.5 * (U(i-1,sj,sk) + U(i,sj,sk)) * DIST_SCALE;
     tt(i,sj,sk) = tt(i-1,sj,sk) + delay
  end do

  !! traverse y axis
  !
  do j = sj-1, 1, -1
     delay = 0.5 * (U(si,j+1,sk) + U(si,j,sk)) * DIST_SCALE;
     tt(si,j,sk) = tt(si,j+1,sk) + delay
  end do
  do j = sj+1, ny
     delay = 0.5 * (U(si,j-1,sk) + U(si,j,sk)) * DIST_SCALE;
     tt(si,j,sk) = tt(si,j-1,sk) + delay
  end do

  !! traverse x axis
  !
  do k = sk-1, 1, -1
     delay = 0.5 * (U(si,sj,k+1) + U(si,sj,k)) * DIST_SCALE;
     tt(si,sj,k) = tt(si,sj,k+1) + delay
  end do
  do k = sk+1, nz
     delay = 0.5 * (U(si,sj,k-1) + U(si,sj,k)) * DIST_SCALE;
     tt(si,sj,k) = tt(si,sj,k-1) + delay
  end do

end subroutine calc_linear_paths_padded

subroutine write_results(nx, ny, nz, tt)
  implicit none
  integer, intent(in) :: nx, ny, nz
  real,    intent(in) :: tt(nx,ny,nz)
  integer :: i, j, k

  open(unit=1, file='output.tt', action='write')

  do i = 1, nx
     do j = 1, ny
        do k = 1, nz
           write (1,'(3i4,f9.4)'), i,j,k, tt(i,j,k)
        end do
     end do
  end do

  close(unit=1)

end subroutine write_results

subroutine write_results_padded(padNx,padNy,padNz, nx,ny,nz, tt)
  implicit none
  integer, intent(in) :: padNx, padNy, padNz, nx, ny, nz
  real,    intent(in) :: tt(padNx,padNy,padNz)
  integer :: i, j, k

  open(unit=1, file='output.tt', action='write')

  do i = 1, nx
     do j = 1, ny
        do k = 1, nz
           write (1,'(3i4,f9.4)'), i,j,k, tt(i,j,k)
        end do
     end do
  end do

  close(unit=1)

end subroutine write_results_padded

end module forward_star

