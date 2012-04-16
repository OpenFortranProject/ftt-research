module test
  implicit none
  contains
  subroutine f
    integer :: x = 4
    call h(x)
    print *,x
  end subroutine f
  
  subroutine h(x)
    integer, intent(in) :: x
    call g(x)
  end subroutine h
  
  subroutine g(x)
    integer, intent(inout) :: x
  
    x = x + 1
  end subroutine g
end module

program main
  use test
  call f
  call f
  call f
end program main
