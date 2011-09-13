!
! -----  Fortran Programming Language with local extensions -----
!
! This file is distributed under an open source license by Los Alamos
! National Security, LCC.  See the file License.txt (located in the
! top level of the source distribution) for details.
! 
!-----
!
! Simplistic 2D heat transfer...
! 
!

module heat_transfer

   integer, parameter :: NPAD = 2
   integer, parameter :: WIDTH  = 1024
   integer, parameter :: HEIGHT = 1024
   integer, parameter :: NX = WIDTH  + 2*NPAD
   integer, parameter :: NY = HEIGHT + 2*NPAD

   integer, parameter :: NTIME_STEPS = 1000
   real,    parameter :: MAX_TEMP    = 100.0

   interface
      pure elemental function adv_time(T)
         real, intent(in) :: T
      end function adv_time
   end interface

contains

!------------------------
! Proposed syntax for the concurrent function adv_time
!------------------------
!
!   pure elemental function adv_time(T)
!      real, EXTEND, intent(in)  :: T(-1:1,-1:1)
!      real :: adv_time
!      real :: lhs1, lhs2
!
!       lhs1 = ( T(-1,0) - 2.0*T(0,0) + T(+1,0) ) / (dx*dx)
!       lhs2 = ( T(0,-1) - 2.0*T(0,0) + T(0,+1) ) / (dy*dy)
!
!      adv_time = (alpha * dt * (lhs2 + lhs2)) + T(0,0)
!
!   end function adv_time
    
end module heat_transfer


program test_heat2d
   use :: heat_transfer
   implicit none
   integer :: i

   real, dimension(0:NX-1,0:NY-1) :: T1, T2
  
   real, parameter :: alpha = .001
   real, parameter :: dx = 1.0 / WIDTH
   real, parameter :: dy = 1.0 / HEIGHT
   real, parameter :: dt = .01 * (alpha / 4.0) * ((1.0 / (dx*dx)) + (1.0 / (dy*dy)))

   ! initialize cell temperature
   !
   T1 = MAX_TEMP
   T2 = 0.0

   T1(:,0)    = 0.0
   T1(:,NY-1) = 0.0
   T1(0,:)    = 0.0
   T1(NX-1,:) = 0.0

   ! time loop
   !
   do i = 1, NTIME_STEPS/2
      T2(1:WIDTH,1:HEIGHT) = adv_time(T1(1:WIDTH,1:HEIGHT))
      T1(1:WIDTH,1:HEIGHT) = adv_time(T2(1:WIDTH,1:HEIGHT))
   end do

end program test_heat2d
