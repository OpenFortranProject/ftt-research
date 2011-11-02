subroutine f(x,y)
  integer :: x
  integer :: y

  print *, x
  print *, y
end subroutine f

program test
  call f(1,2)
end program
