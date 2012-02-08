!===========================================================================
!
! Kernel function that performs the convolution for the array element
! specified by indices.
!
!===========================================================================

!$OFP CONCURRENT :: convolve
pure subroutine convolve(Image, Filter, H)

!$OFP HALO(:,:) :: Image(0:,0:)
   integer, intent(in)   :: H
   real, intent(in out)  :: Image(-H:,-H:)
   real, intent(in)      :: Filter(-H:H,-H:H)

   Image(0,0) = sum(Filter * Image(-H:+H,-H:+H))

end subroutine convolve
