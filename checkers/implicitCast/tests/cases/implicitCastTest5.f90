! Test that we catch the implicit cast of
! a function's returned value in the presence
! of explicit interfaces

function f()
  real :: f
  real :: x = 42.4

  f = x
end function

program test
  interface
    function f()
    real :: f
    end function
  end interface

  INTEGER :: x
  
  x = f() + 4
  
  print *, x

end
