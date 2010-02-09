subroutine f(a,b)
  implicit integer (a,b)
end subroutine f

program test27
  integer :: x,y

  call f(x,y)
end program test27
