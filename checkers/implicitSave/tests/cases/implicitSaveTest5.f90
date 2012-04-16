module test
  implicit none
  contains
  subroutine h
    integer, save :: x = 4
    print *,x
  end subroutine
end module

program main
  use test
  print *, "Testing h"
  call h
  call h
  call h
end program main
