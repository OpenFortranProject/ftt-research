program test13
  integer :: x,y

  x = 14

  if (x == 12) then
     y = 12
  else if (x == 4) then
     y = 13
  else if (x == 5) then
     y = 15
  else 
     y = 16
  end if
end program test13
