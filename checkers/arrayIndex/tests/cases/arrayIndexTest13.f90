program main
  integer :: counter
  real, dimension(5) :: x
  do counter = 1,5,1
    write(*,'(i2)') x(counter)
  end do

end program
