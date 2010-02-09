subroutine iterate(x)
  real, intent(inout), dimension(:,:) :: x
  integer :: m,n,i,j

  do i=2,(m-1)
     do j=2,(n-1)
        x(i,j) = (x(i-1,j) + x(i+1,j) + x(i,j-1) + x(i,j+1) + x(i,j)) / 5.0
     end do
  end do
end subroutine iterate

program tester
  interface
     subroutine iterate(x)
       real, intent(inout), dimension(:,:) :: x
     end subroutine iterate
  end interface
  real, dimension(10,10) :: x

  x(1,:) = 100.0;
  x(2:10,:) = 0.0;

  call iterate(x)
end program tester
