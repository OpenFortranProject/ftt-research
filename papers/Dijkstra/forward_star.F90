module forward_star

contains

subroutine read_forward_star(nfs, off)
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
  integer, intent(in ) :: nfs
  integer, intent(in ) :: off(3,nfs)
  real,    intent(out) :: dist(nfs)
  integer :: i, oi, oj, ok

  do i = 1, nfs
     oi = off(1,i)
     oj = off(2,i)
     ok = off(3,i)
     dist(i) = sqrt( REAL(oi*oi + oj*oj + ok*ok) );
  end do

end subroutine calc_distance

subroutine write_results(nx, ny, nz, tt)
  integer, intent(in ) :: nx, ny, nz
  real,    intent(out) :: tt(nx,ny,nz)
  integer :: i, j, k

  open(unit=1, file='output.tt')

  do i = 1, nfs
     write (1,'(3i4,f8.3)'), i,j,k, tt(i,j,k)
  end do

  close(unit=1)

end subroutine write_results

end module forward_star

