pure elemental function elemental_add(a, b) result(c)
   real :: a, b, c
   intent(in) :: a, b

   c = a + b

end function elemental_add

pure subroutine fold(f, A, B, C)
   real, dimension(:,:,:), intent(in)  :: A, B
   real, dimension(:,:,:), intent(out) :: C

   interface
      pure elemental function f(a, b)
         real :: a, b, f
         intent(in) :: a, b
      end function
   end interface

   C = f(A, B)   

   !
   ! OpenCL code
   !
   ! c[k] = f(a[k] + b[k])

end subroutine fold

