! compute mean of a set of numbers
FUNCTION MEAN(x, nx)
  integer :: nx, i
  real, dimension(nx) :: x, mean, tot

  tot = 0.0
  do 5 i=1,nx,7
     tot = tot + x(i)
5 continue
  mean = tot/real(nx)
end function mean

