program operators
  real :: x
  real :: y
  real :: z

  if (x == y) then
      x = y + z
  end if
  if (x < y) then
      x = y - z
  end if
  if (x <= y) then
      x = y * z
  end if
  if (x > y) then
      x = y / z
  end if
  if (x >= y) then
      x = y ** z
  end if
  if ( x /= y) then
      x = y ** (1/z)
  end if

end program
