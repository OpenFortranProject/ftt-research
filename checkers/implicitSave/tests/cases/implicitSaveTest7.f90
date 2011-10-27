subroutine f
  integer :: x = 4
  call g(x)
  print *,x
end subroutine f

subroutine g(x)
  integer, intent(inout) :: x

  x = x + 1
end subroutine g

program main
  call f
  call f
  call f
end program main
