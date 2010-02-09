program test23
  integer, dimension(5) :: y
  integer :: x(5)
  integer :: i

  y(:) = 1

  do i=1,5
    x(i) = y(i)+i
  end do

  print *,'y=',y
  print *,'x=',x
end program test23
