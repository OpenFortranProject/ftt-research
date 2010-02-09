subroutine foo(x,y)
  integer :: x
  real :: y

  print *,x,y
end subroutine foo

program test34
  integer :: a = 99
  real :: b = 3.14

  interface
    subroutine foo(x,y)
      integer :: x
      real :: y
    end subroutine foo
  end interface

  call foo(y=b, x=a)
end program test34
