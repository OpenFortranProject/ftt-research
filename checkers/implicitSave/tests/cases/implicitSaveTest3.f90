subroutine h
  integer :: x = 4
  print *,x
end subroutine

program main
  print *, "Testing h"
  call h
  call h
  call h
end program main
