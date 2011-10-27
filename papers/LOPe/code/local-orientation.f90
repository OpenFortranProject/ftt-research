! one dimensional advection with constant velocity
!
!         h_t + u_x = 0
!
! where u = ah
!
!
pure subroutine advect(H_inout, halo, a, dx, dt)
   !$OFP EXTENDED ELEMENTAL advect
   real, intent(inout) :: H_in     ! will be an array, includes the halo region
   !$OFP EXTENDED H_in
   real, intent(in) :: a, dx, dt
   integer, intent(in) :: halo(2)  ! halo/ghost cell size of H_in
                                   ! this can probably be supplied by the compiler

   real :: u_west, u_east
   real :: H(-1:1), U(-1:1)
   integer :: i

    H = extended_section(H_in, halo, [1,1])   ! take 3, 1 on either side
    U = a*H

   ! Lax step (t+dt/2)
   !
   i = 0
   u_west = 0.5*a* ( H(i-1) + H(i  ) ) + (0.5*dt/dx) * ( U(i-1) - U(i  ) )
   u_east = 0.5*a* ( H(i  ) + H(i+1) ) + (0.5*dt/dx) * ( U(i  ) - U(i+1) )

   ! second Lax-Wendroff step (t+dt)
   !
   H_inout = H_inout + (0.5*dt/dx) * (u_west - u_east)

end subroutine advect
