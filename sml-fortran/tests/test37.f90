! compute mean of a set of numbers
FUNCTION MEAN(x, nx)
  integer :: nx, i
  real, dimension(nx) :: x, mean, tot

  tot = 0.0
  do i=1,nx
     tot = tot + x(i)
  end do
  mean = tot/real(nx)
end function mean

