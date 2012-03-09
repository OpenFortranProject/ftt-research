! Test that we detect (and warn) about the
! conversion of real to integer.
program test

  INTEGER :: x
  REAL    :: y
  
  y = 42.4
  x = y + 4.0
  
  print *, x
  print *, y

end
