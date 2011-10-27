!
! I'm not sure about overloaded functions and function names for these.
! There will potentiall be several versions, depending on scale factor.
!

! for now this reduction assumes a 2x2 expansion

Module ForOpenCL

interface

   subroutine reduce(f, A_big, A)
      real, dimension(:,:) :: A_big, A
      interface
         pure elemental function f(a, b)
            real :: a, b, f
            intent(in) :: a, b
         end function
      end interface
   end subroutine reduce

   subroutine scatter(A, A_big)
      real, dimension(:,:) :: A, A_big
   end subroutine scatter

end interface

end module ForOpenCL


