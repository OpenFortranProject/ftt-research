!
! I'm not sure about overloaded functions and function names for these.
! There will potentiall be several versions, depending on scale factor.
!

Module ForOpenCL

interface local
  module procedure local_1d, local_2d
end interface

interface local_region
  module procedure local_region_1d, local_region_2d
end interface

contains

   pure elemental real function identity(a)
      real, intent(in) :: a
      identity = a
   end function identity

   pure elemental real function elemental_sum(a, b)
      real, intent(in) :: a, b
      elemental_sum = a + b
   end function elemental_sum

   !
   ! local()
   ! 
   ! Returns a reference (pointer for now) to the local
   ! array address.  As such it is unsafe until the language
   ! semantics in Fortran it.  It is meant to only
   ! be used in a definition context so it should be pure (?), e.g.,
   !
   !      local(A) = 1
   ! 
   ! This assigns one to the local address only.  This is different
   ! from normal array syntax
   !
   !      A = 1
   !
   ! Which broadcasts 1 to all array elements.   
   !
   ! This is a language extension and doesn't work correctly in f90
   !
   function local_1d(A)
      implicit none
      real, target, intent(inout) :: A(:)
      real, pointer :: local_1d
      local_1d => A(1)      ! the best we can do for now
   end function local_1d

   function local_2d(A)
      implicit none
      real, target, intent(inout) :: A(:,:)
      real, pointer :: local_2d
      local_2d => A(1,1)      ! the best we can do for now
   end function local_2d

   !
   ! local_region()
   !
   ! Returns copy of an array section from a local (elemental) perspective
   !
   pure function local_region_1d(A, local_expand, halo)
      implicit none
      real, intent(in) :: A(0:)
      real, pointer :: local_region_1d(:)
      integer, intent(in), dimension(2) :: local_expand, halo
      optional :: halo
      local_region_1d = A(:local_expand(1)+local_expand(2))
   end function local_region_1d

   pure function local_region_2d(A, local_expand, halo)
      implicit none
      real, intent(in) :: A(0:,0:)
      real, pointer :: local_region_2d(:,:)
      integer, intent(in), dimension(4) :: local_expand, halo
      optional :: halo
      local_region_2d = A(:local_expand(1)+local_expand(2), :local_expand(3)+local_expand(4))
   end function local_region_2d

!
! Should these be functions or subroutines.  I think functions where possible
!
   pure function scatter(f, A, scale) result(A_big)
      implicit none
      real, dimension(:,:), intent(in)  :: A
      real, dimension(:,:), allocatable :: A_big
      integer, intent(in) :: scale
      integer :: i
      interface
         pure elemental real function f(a)
            real, intent(in) :: a
         end function
      end interface

      do i = 1, scale
         A_big(i::scale, i::scale) = f(A)
      end do

   end function scatter

   pure function reduce(f, A_big, scale) result(A)
      real, dimension(:,:), intent(in)  :: A_big
      real, dimension(:,:), allocatable :: A
      integer, intent(in) :: scale
      interface
         pure elemental function f(a, b)
            real :: a, b, f
            intent(in) :: a, b
         end function
      end interface

      A = 0
      do i = 1, scale
         A = f(A, A_big(i::scale, i::scale))
      end do

   end function reduce

   pure subroutine scatter_sub(f, A, A_big, scale)
      real, dimension(:,:), intent(in)  :: A
      real, dimension(:,:), intent(out) :: A_big
      integer, intent(in) :: scale
      integer :: i
      interface
         pure elemental real function f(a)
            real, intent(in) :: a
         end function
      end interface

      do i = 1, scale
         A_big(i::scale, i::scale) = f(A)
      end do
   end subroutine scatter_sub

   pure subroutine reduce_sub(f, A_big, A, scale)
      real, dimension(:,:), intent(in)  :: A_big
      real, dimension(:,:), intent(out) :: A
      integer, intent(in) :: scale
      interface
         pure elemental function f(a, b)
            real :: a, b, f
            intent(in) :: a, b
         end function
      end interface

      A = 0
      do i = 1, scale
         A = f(A, A_big(i::scale, i::scale))
      end do
   end subroutine reduce_sub

end module ForOpenCL


