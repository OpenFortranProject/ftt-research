subroutine f
  implicit none
  integer :: x = 4
  call h(x)
  print *,x
end subroutine f

subroutine h(x)
  implicit none
  interface
    subroutine g(x)
      integer, intent(in) :: x
    end subroutine
  end interface
  integer, intent(in) :: x
  call g(x)
end subroutine h

subroutine g(x)
  implicit none
  integer, intent(inout) :: x

  x = x + 1
end subroutine g

program main
  implicit none
  call f
  call f
  call f
end program main
