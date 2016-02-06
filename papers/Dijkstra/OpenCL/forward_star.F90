module forward_star

  real, parameter :: DIST_SCALE = 10.0   ! grid size of 10 km

contains

subroutine read_forward_star(nfs, off)
  implicit none
  integer, intent(in ) :: nfs
  integer, intent(out) :: off(3,nfs)
  integer :: i

  open(unit=1, file='arcList.txt')

  do i = 1, nfs
     read(1, *) off(1,i), off(2,i), off(3,i)
  end do

  close(unit=1)

end subroutine read_forward_star

subroutine read_velocity_model(nx, ny, nz, U)
  implicit none
  integer, intent(in ) :: nx, ny, nz
  real,    intent(out) :: U(nx,ny,nz)
  integer :: i, j, k, oi, oj, ok

  open(unit=1, file='srModel_forTomographyPaper.txt')

  do i = 1, nx
     do j = 1, ny
        do k = 1, nz
           read(1, *) oi, oj, ok, U(i,j,k)
        end do
     end do
  end do

  close(unit=1)

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

end module forward_star

