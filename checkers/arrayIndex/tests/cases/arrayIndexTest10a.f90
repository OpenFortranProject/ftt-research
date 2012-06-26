program main

  integer :: x(4)
  integer :: y
  integer :: z

  y = 1
  y = 2
  if (y < size(x)) then
    z = 7
  else
    z = 8
  endif

  x(y) = 1
  x(y) = 2

end program main
