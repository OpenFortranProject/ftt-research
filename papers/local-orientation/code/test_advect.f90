program test_advect
   implicit none
   integer, parameter :: NX = 16
   integer :: i
   real :: H(0:NX+1), a, dx, dt

   interface
       pure subroutine advect_impl(H_big, halo, a, dx, dt)
          real, dimension(*), intent(inout) :: H_big   ! includes the halo region
          real, intent(in) :: a, dx, dt
          integer, intent(in) :: halo(2)
       end subroutine
   end interface

   integer :: nLoops = 500

   a  = 2.0
   dx = 1.0
   dt = 0.1 * (dx/a)

   ! boundary conditions
   !
!   H(0)    = 1.0
!   H(NX+1) = 0.0
   H = 1.0

   ! interior
   !
!   do i = 1, NX
!      H(i) = 1.0 - (i/((NX+1)*dx))
!   end do
!   H(NX+1) = H(NX)    ! outflow boundary conditions

   print *
   print *, "initial conditions"
   print *, H

   do i = 1, nLoops
      call advect_impl(H, [1,1], a, dx, dt)
!      H(NX+1) = H(NX)    ! outflow boundary conditions
   end do

   print *
   print *, "final conditions"
   print *, H

end program
