function f(x)
  integer :: f
  integer :: x

  f = x
end function

program test
  interface
    function f(x)
    integer :: f
    integer :: x
    end function
  end interface

  real    :: x
  integer :: y

  x = 3.14

  y = f(x)
  
  print *, y

end
