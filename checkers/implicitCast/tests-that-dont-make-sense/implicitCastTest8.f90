subroutine f(x)
  integer, intent(out) :: x

  x = 42
end subroutine

program test
  interface
    subroutine f(x)
    integer, intent(out) :: x
    end subroutine
  end interface

  real    :: x

  x = 3.14

  call f(x)
  
  print *, x

end
