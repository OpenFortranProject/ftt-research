   CONCURRENT pure elemental subroutine convolve(SmoothedImage, Image, F, N)
      real, intent(out) :: SmoothedImage
      real, intent(in), EXTEND(0) :: Image(-N:N,-N:N)
      real, intent(in), NON_ELEMENTAL :: F(-N:N,-N:N)
      integer, intent(in) :: N

      SmoothedImage = sum(F*Image)

   end subroutine convolve
