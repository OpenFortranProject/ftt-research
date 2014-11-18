!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Transformations needed:
!
!   1. Remove [device] and [[device]]
!   2. Add (j2h:j2h), for example
!

program test_restrict
   use restriction

   integer, parameter :: J = 256  ! r dimension
   real :: U1h(0:J)               !  J-1   interior points, HALO(-1:*:1)
   real :: U2h(0:J/2)             !  J/2-1 interior points, HALO(-1:*:1)
   real :: error
   integer :: j1h, j2h

   do j1h = 0, J
      U1h(j1h) = j1h
   end do

   do concurrent (j2h = 1:J/2-1)
      call restrict(U1h(2*j2h:2*j2h), U2h(j2h:j2h))
   end do

   do j2h = 1, J/2-1
      error = 2*j2h - U2h(j2h)
      if (abs(error) > 1.e-6) then
         print *, "ERROR:", 2*j2h, U2h(j2h)
      end if
   end do
         
   do concurrent (j2h = 1:J/2-1)
      call restrict(U1h(2*j2h:2*j2h), U2h(j2h:j2h), [0.25, 0.5, 0.25])
   end do

   do j2h = 1, J/2-1
      error = 2*j2h - U2h(j2h)
      if (abs(error) > 1.e-6) then
         print *, "ERROR:", 2*j2h, U2h(j2h)
      end if
   end do
         
end program
