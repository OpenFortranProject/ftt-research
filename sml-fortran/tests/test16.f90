program test16
  integer :: i,j
  real, dimension(10,10) :: x,y

  x(:,:) = 4.0
  y(1:4,:) = 3.0
  y(5:10,:) = 6.0

  do i=1,10
     x(i,:) = y(:,i)
  end do

  do i=1,10
     do j=1,10
        x(i,j) = y(j,i) * 1.5
     end do
  end do

  do i=1,9
     x(i,:) = y(i+1,:)
  end do
end program test16
