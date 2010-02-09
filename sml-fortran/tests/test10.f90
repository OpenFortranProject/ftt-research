program test10
  integer :: i,j,k,l

  j = 0

  do i=1,100,5
     j = j + i
     k = (j*i)+10
     l = k-j
  end do
end program test10
