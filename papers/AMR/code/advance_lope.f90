!===========================================================================
!
! Concurrent (kernel) function that performs time step.
!
!===========================================================================

pure CONCURRENT subroutine advance(State)
   type(State_SW), intent(in out), HALO(
   real, intent(in out), HALO(:,:), HALO_FUNCTION(user_defined_function) :: Image(:,:)
   real, intent(in)     :: Filter(-H:H,-H:H)
   integer, intent(in)  :: indices(2)

   ! local variables
   integer :: i, j
   i = indices(1)
   j = indices(2)

   Image(i,j) = sum(Filter * Image(i-H:i+H,j-H:j+H))

end subroutine convolve

end module Convolve_AMR


program convolve_image
   use :: Convolve_AMR
   integer :: i, j

   ! initialize Filter and Image coarray data on this image
   !
   call initialize

   ! call intrinsic function to exchange halo data between images
   ! Note to Bob: Do you need to supply your own function for the
   ! exchange of halos?
   !
   call exchange_halo(Image)

   do concurrent (i=1:N, j=1:M)
      call convolve(Image, Filter)   ! indices supplied by compiler
   end do

   ! output local data (including coarrays)
   !
   call output_image

end program convolve_image
