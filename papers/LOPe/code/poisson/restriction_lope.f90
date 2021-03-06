!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Transformations needed:
!
!   1. Remove CONCURRENT
!   2. Transform HALO(:) to DIMENSION(0:)
!

Module Restriction

interface restrict
   module procedure restrict_1D, restrict_1D_convolve
end interface restrict

Contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Called with:
!
!  !! iterate over coarse grid
!  !
!  do concurrent (i2h = 1:N/2-1) [[device]]
!     call restrict(U1h(2*i2h)[device], U2h(i2h)[device])
!     call restrict(U1h(2*i2h)[device], U2h(i2h)[device], [0.25, 0.5, 0.25])
!  end do
!
pure subroutine restrict_1D(U1h, U2h)
   real, intent(in ), dimension(0:) :: U1h   ! fine grid
   real, intent(out), dimension(0:) :: U2h   ! coarse grid

   U2h(0) = 0.25*(U1h(-1) + 2.0*U1h(0) + U1h(+1))

end subroutine restrict_1D

!! Apply convolution filter/stencil S = [0.25, 0.5, 0.25]
!
pure subroutine restrict_1D_convolve(U1h, U2h, S)
   real, intent(in ), dimension(0:) :: U1h   ! fine grid
   real, intent(out), dimension(0:) :: U2h   ! coarse grid
   real, intent(in )                :: S(3)  ! stencil

   U2h(0) = sum( S*U1h(-1:1) )

end subroutine restrict_1D_convolve

End Module Restriction
