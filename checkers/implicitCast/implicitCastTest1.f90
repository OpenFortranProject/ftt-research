program test

  INTEGER :: x
  REAL    :: y
  
  y = 42.4
  x = INT(y) + 4
  
  print *, x
  print *, y

end
