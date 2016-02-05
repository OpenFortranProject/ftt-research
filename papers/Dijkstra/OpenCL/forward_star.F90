module forward_star

contains

subroutine read_forward_star(nfs, off, dist)
  integer, intent(in ) :: nfs
  integer, intent(out) :: off(3,nfs)
  real,    intent(out) :: dist(nfs)
  integer :: i, oi, oj, ok

  open(unit=1, file='arcList.txt')

  do i = 1, nfs
     read(1, *) oi, oj, ok
     off(1,i) = oi
     off(2,i) = oj
     off(3,i) = ok
     dist(i) = sqrt( REAL(oi*oi + oj*oj + ok*ok) );
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

end module forward_star

