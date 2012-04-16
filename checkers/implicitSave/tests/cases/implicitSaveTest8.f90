module test
  implicit none
  contains
  subroutine g
    integer :: x
    x = 4
    print *, x
    x = x + 1
  end subroutine g
end module

program main
  use test
  print *, "Testing g"
  call g
  call g
  call g
end program main
