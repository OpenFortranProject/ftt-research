module shallow_mod

CONTAINS

subroutine shallow(is, ie, js, je, ks, ke, velocity)
!$OFP kernel procedure :: shallow
   implicit none
   integer :: is, ie, js, je, ks, ke
   real, dimension(is:ie, js:je, ks:ke) :: velocity

   integer :: i, j, k

   DO CONCURRENT (i=is:ie, j=js:je, k=ks:ke)
   BLOCK         

      ! local variable declarations (mostly scalar)
      !
      real :: l_vel

      ! load local variables (into registers) from arrays
      !
      l_vel = velocity(i,j,k)

      ! do computation using local variables
      !
      l_vel = l_vel/2.0

      ! store computed values back to arrays
      !
      velocity(i,j,k) = l_vel

   END BLOCK
   END DO CONCURRENT

end subroutine shallow

end module shallow_mod


program test_shallow
   use shallow_mod
   implicit none

   integer, parameter :: SIZE = 64   
   real, dimension(SIZE, SIZE, SIZE) :: velocity
   integer :: is, ie, js, je, ks, ke

   is = 1;    js = 1;    ks = 1;
   ie = SIZE; je = SIZE; ke = SIZE

   velocity = 1.0

   call shallow(is, ie, js, je, ks, ke, velocity)

end program
