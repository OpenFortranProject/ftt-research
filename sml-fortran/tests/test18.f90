program foo
  integer :: a,b,c,d,e,f=17
  real :: data(1000)
  integer :: i,j
  
  a = 12
  b = 13+a
  c = 14*(a/b)
  d = (a+b)**c + f
  e = (a+b-c)*(-d+e**f)

  do i=1,499,5
    do j = 500,1000
       data(i+j) = a**b + c**d
    end do
  end do
end program foo
