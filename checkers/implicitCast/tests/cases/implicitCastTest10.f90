! Test that we detected implicit cast
! when assigning function return value
! in the presence of explicit interface

function f()
  integer :: f
  real :: x = 42.4

  f = x
end function

program test
  interface
    function f()
    integer :: f
    end function
  end interface

  INTEGER :: x
  
  x = f() + 4
  
  print *, x

end
