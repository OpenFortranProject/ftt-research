!
! I'm not sure about overloaded functions and function names for these.
! There will potentiall be several versions, depending on scale factor.
!

! for now this scatters assuming a 2x2 expansion
! I'll provide implementation later

Module ForOpenCL

interface
   subroutine scatter(A, A_big)
      real, dimension(:,:) :: A, A_big
   end subroutine scatter
end interface

end module ForOpenCL
