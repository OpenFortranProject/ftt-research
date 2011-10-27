!
! computes derivative of A using centered difference
!
! I believe A_prime(i) = ( A(i+1) - A(i-1) ) / (2*dx)
!
! We use a local orientation
!
subroutine finite_difference(A, A_prime, dx)
   use :: ForOpenCL
   implicit none
   real, dimension(:) :: A, A_prime
   real :: dx, A_local(-1:1)
   real, pointer :: A_prime_local          ! this will go away

   A_local = local_region(A, [1,1,1,1], halo=[1,1,1,1])   ! A needs halo specification
   A_prime_local => local(A_prime)

   A_prime_local = ( A_local(1) - A_local(-1) ) / (2*dx)

end subroutine finite_difference
