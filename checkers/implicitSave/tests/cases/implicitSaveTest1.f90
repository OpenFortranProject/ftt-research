module test
  implicit none
  contains
  subroutine f
    integer :: x = 4
    print *, x
    x = x + 1
  end subroutine f
end module

program main
  use test
  print *, "Testing f"
  call f
  call f
  call f
end program main
