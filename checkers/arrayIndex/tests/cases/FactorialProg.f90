program FactorialProg
  integer :: counter = 5
  integer :: factorial = 1
  do
    factorial = factorial * counter
    counter = counter - 1
    if (counter == 0) exit
  end do
  print *, factorial
end program FactorialProg
