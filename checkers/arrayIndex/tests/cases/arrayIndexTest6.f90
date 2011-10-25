subroutine f(x)
  integer, dimension(:) :: x

  x(1) = 1
end subroutine

program main

  integer :: x(4)

  call f(x)

end program main
