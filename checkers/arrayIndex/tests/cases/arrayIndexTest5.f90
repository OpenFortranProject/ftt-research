program main

  integer :: x(4)
  integer :: y = 1
  integer :: z = size(x)

  if (z >= y) then
    x(y) = 1
  end if

end program main
