! This checks that we notice the implicit cast of the
! the function's returned value.

function f()
  real :: x = 42.4

  f = x
end function

program test

  INTEGER :: x
  
  x = f() + 4
  
  print *, x

end
