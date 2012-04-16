! Make sure we don't warn about
! explicit cast of y to integer.
program test

  INTEGER :: x
  REAL    :: y
  
  y = 42.4
  x = INT(y) + 4
  
  print *, x
  print *, y

end
