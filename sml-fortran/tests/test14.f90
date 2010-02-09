program test14
  integer :: x,y

  x = 1

  if (x.eq.12) then
     y = 1
  end if

  if (y.eq.1) then
     y = x
  else if (y==12) then
     y = 12
  else
     y = 6
  end if
end program test14
