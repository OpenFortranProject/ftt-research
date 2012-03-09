! Test for implicit promotion of x to real
! No warning is expected by default
program test

  INTEGER :: x
  REAL    :: y
 
  x = 4 
  y = 42.4 + x
  
  print *, x
  print *, y

end
