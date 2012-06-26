program main

  integer :: x(4)
  integer :: y

  y = 1
  y = 2
  if (y == 0) then
    y = 7
  else
    y = 8
  endif

  x(y) = 1
  x(y) = 2

end program main
