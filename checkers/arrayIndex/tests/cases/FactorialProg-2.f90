program FactorialProg
  integer :: counter = 5
  integer :: factorial = 1
  factorial = factorial * counter
  counter = counter - 1
  do while (counter > 0)
    factorial = factorial * counter
    counter = counter - 1
  end do
  print *, factorial
end program FactorialProg
