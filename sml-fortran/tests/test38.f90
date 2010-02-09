function foo(x,y)
  integer :: x
  real :: y, foo

  print *,x,y
  foo = y
end function foo

program test34
  integer :: a = 99
  real :: b = 3.14, c

  interface
    function foo(x,y)
      integer :: x
      real :: y, foo
    end function foo
  end interface

  c = foo(y=b, x=a)
end program test34
