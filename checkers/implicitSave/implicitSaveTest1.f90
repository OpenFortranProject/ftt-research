subroutine f
  integer :: x = 4
  print *, x
  x = x + 1
end subroutine f

subroutine g
  integer, save :: x = 4
  print *, x
  x = x +1
end subroutine g

subroutine h
  integer :: x = 4
  print *,x
end subroutine

program main
  print *, "Testing f"
  call f
  call f
  call f
  print *, "Testing g"
  call g
  call g
  call g
  print *, "Testing h"
  call h
  call h
  call h
end program main
