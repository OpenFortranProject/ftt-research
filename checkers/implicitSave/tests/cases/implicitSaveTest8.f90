subroutine g
  integer :: x
  x = 4
  print *, x
  x = x + 1
end subroutine g
program main
  print *, "Testing g"
  call g
  call g
  call g
end program main
