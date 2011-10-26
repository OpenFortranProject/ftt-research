subroutine f
  integer, save :: x = 4
  call h(x)
  print *,x
end subroutine f

subroutine h(x)
  integer, intent(out) :: x
  call g(x)
end subroutine h

subroutine g(x)
  integer, intent(inout) :: x

  x = x + 1
end subroutine g

program main
  call f
  call f
  call f
end program main
