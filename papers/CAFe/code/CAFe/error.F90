Module Error
   Use, intrinsic :: ISO_C_BINDING
   Implicit none

   Real(C_FLOAT), allocatable :: Err(:,:)

contains


Subroutine error_allocate(m, n, h)
   implicit none
   integer, intent(in) :: m, n, h

   print *, "error_allocate: printing m,n,h"
   write(6,*) m,n,h

   allocate(Err(1:m+2*h, 1:n+2*h))

   Err = 0.0

End Subroutine error_allocate


Subroutine calc_error(m, n, h, A)
   Use, intrinsic :: ISO_C_BINDING
   Implicit None
   Integer(C_INT), intent( in), value :: m, n, h
   Real(C_FLOAT ), intent( in)        :: A (1:m+2*h, 1:n+2*h)

   Integer :: i, j

   do j = 1+h, n+h
      do i = 1+h, m+h
         Err(i,j) = (-A(i-1,j) - A(i+1,j) - A(i,j-1) - A(i,j+1)) + 4*A(i,j)
      end do
   end do

   write(6,*) Err(6,6), Err(7,6), Err(6,7)


End Subroutine calc_error

End Module Error
