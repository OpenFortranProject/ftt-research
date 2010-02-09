program test19
  integer :: i
  real :: x(10)

  do i=1,10
     if (i .eq. 1) then
        x(i) = 1.2
     else
        x(i) = x(i-1) * 0.75;
     end if
  end do

  do i=1,10
     if (i .eq. 5) then
        cycle
     else
        x(i) = 0.0 - x(i)
     end if
  end do
end program test19
