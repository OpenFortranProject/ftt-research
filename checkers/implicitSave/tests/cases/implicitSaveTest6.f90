subroutine f
  integer, save :: x
  print *, x
  x = x + 1
end subroutine f

program main
  print *, "Testing f"
  call f
  call f
  call f
end program main
