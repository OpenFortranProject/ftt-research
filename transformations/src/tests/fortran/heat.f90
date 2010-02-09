program heat

  interface
    pure subroutine advance(T, lb, rb)
      real, dimension(:), intent(inout) :: T
      real, intent(in) :: lb, rb
    end subroutine
    pure subroutine initialize(T, lb, rb)
      real, dimension(:), intent(out) :: T
      real, intent(out) :: lb, rb
    end subroutine
  end interface  

  real, dimension(10) :: T
  real :: lb, rb
  integer :: time

  call initialize(T, lb, rb)

  do time = 1, 1000
    call advance(T, lb, rb)
  end do

  print *
  print *, lb, rb
  print *, T

end program


pure subroutine initialize(T, lb, rb)
  real, dimension(:), intent(out) :: T
  real, intent(out) :: lb, rb

  lb = 0.0
  rb = 100.0
  T = 0.0

end subroutine initialize


elemental real function halve(x)
  real, intent(in) :: x
  halve = x/2.0
end function halve


!
! 1D heat flow
!
pure subroutine advance(T, lb, rb)
  include "halve.h"

  real, dimension(:), intent(inout) :: T
  real, intent(in) :: lb, rb

  T = halve(  eoshift(T, shift=-1, boundary=lb) &
            + eoshift(T, shift= 1, boundary=rb) &
           )

end subroutine advance
