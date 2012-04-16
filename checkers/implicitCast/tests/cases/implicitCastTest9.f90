! Test that we detect when function return type
! doesn't match function return value

function f()
  integer :: f
  real :: x
  x = 42.4
  f = x
end function

program test

  INTEGER :: x
  
  x = f() + 4
  
  print *, x

end
