!
!-----  Fortran Programming Language with local extensions -----
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
      CONCURRENT subroutine adv_time_lope(T)
         real, intent(in out) :: T(:,:)
      end subroutine adv_time_lope
   end interface

end module heat_transfer


program test_heat2d
   use :: heat_transfer
   implicit none
   integer :: i

   real, allocatable, dimension(:,:) :: T
  
   real, parameter :: alpha = .001
   real, parameter :: dx = 1.0 / WIDTH
   real, parameter :: dy = 1.0 / HEIGHT
   real, parameter :: dt = .01 * (alpha / 4.0) * ((1.0 / (dx*dx)) + (1.0 / (dy*dy)))

   ! allocate memory
   !
   allocate(T(HEIGHT,WIDTH), HALO=(1,1))

   ! initialize cell temperature
   !
   T = MAX_TEMP

   ! need to figure out syntax for setting halo regions
   !

   ! time loop
   !
   do i = 1, NTIME_STEPS
      call adv_time_lope(T)
   end do

end program test_heat2d
