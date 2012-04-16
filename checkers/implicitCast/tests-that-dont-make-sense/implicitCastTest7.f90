subroutine f(y)
  real, intent(out) :: y
  y = 42.4
end subroutine

program test
  interface
    subroutine f(y)
    real :: y
    end subroutine
  end interface

  INTEGER :: x
  
  call f(x)
  
  print *, x

end
