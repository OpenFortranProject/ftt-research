!
! one dimensional advection with constant velocity
!
!         h_t + u_x = 0
!
! where u = ah
!
!
!pure subroutine advect(H_big, halo, a, dx, dt)
!   !$OFP LOCAL advect
!   real, dimension(:), intent(inout) :: H_big   ! includes the halo region
!   real, intent(in) :: a, dx, dt
!   integer, intent(in) :: halo(2)
!
!   real :: u_west, u_east
!   real :: iH, H(-1:1), U(-1:1)
!   integer :: i
!
!   iH = local(H, halo)                      ! local variables are copies
!    H = local_section(H_big, halo, [1,1])   ! take 3, 1 on either side
!    U = a*H
!
!   ! Lax step (t+dt/2)
!   !
!   i = 0
!   u_west = 0.5*a* ( H(i-1) + H(i  ) ) + (0.5*dt/dx) * ( U(i-1) - U(i  ) )
!   u_east = 0.5*a* ( H(i  ) + H(i+1) ) + (0.5*dt/dx) * ( U(i  ) - U(i+1) )
!
!   ! second Lax-Wendroff step (t+dt)
!   !
!   iH = iH + (0.5*dt/dx) * (u_west - u_east)
!
!   ! local variable iH stored back to array H_big at exit
!end subroutine advect


!
! implementation
!
subroutine advect_impl(H_big, halo, a, dx, dt)
   implicit none
   integer, parameter :: NX = 16
   real, dimension(NX+2), intent(inout) :: H_big   ! includes the halo region
   real, intent(in) :: a, dx, dt
   integer, intent(in) :: halo(2)

   real :: u_west, u_east, diff
   real :: H(-1:1), U(-1:1)
   integer :: i, iH

   ! loop over interior
   !
   do i = 1, NX
      iH = i + halo(1)
       H = H_big(iH-1:iH+1)
       U = a*H

      ! Lax step (t+dt/2)
      !
      u_west = 0.5*a* ( H(-1) + H(0) ) - (0.5*dt/dx) * ( U(-1) - U(0) )
      u_east = 0.5*a* ( H( 0) + H(1) ) - (0.5*dt/dx) * ( U( 0) - U(1) )

      ! second Lax-Wendroff step (t+dt)
      !
      diff = (0.5*dt/dx) * (u_west - u_east)
      H_big(iH) = H_big(iH) + diff

!      if (i == 1) then
!         print *
!         print *, H
!         print *, 0.5 * (H(-1) + H(0))
!         print *, 0.5 * (H( 0) + H(1))
!         print *, (0.5*a*dt/dx) * ( H(-1) - H(0) )
!         print *, (0.5*a*dt/dx) * ( H( 0) - H(1) )
!         print *, flux_west, flux_east, diff
!      end if

   end do

end subroutine advect_impl

