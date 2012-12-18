program jacobi
  integer                  :: n = 100
  real, dimension(100,100) :: x,tmp
  integer                  :: i,j
  real                     :: eps

  x = 0.0
  x(:,1) = 100.0

  eps = 1.0

  print *,eps
  do while (eps > 0.001)
    tmp(:,1) = x(:,1)
    tmp(1,:) = x(1,:)
    tmp(:,n) = x(:,n)
    tmp(n,:) = x(n,:)

    do i=2,(n-1)
      do j=2,(n-1)
        tmp(i,j) = ( x(i-1,j) + x(i+1,j) + x(i,j-1) + x(i,j+1) ) / 4.0
      end do
    end do
    eps = sqrt(sum((x-tmp)*(x-tmp)));
    x = tmp
    print *,eps
  end do

end program jacobi
