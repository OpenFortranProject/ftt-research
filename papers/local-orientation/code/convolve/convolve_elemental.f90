   CONCURRENT elemental subroutine convolve(S, Image, F, N)
      real, intent(out) :: S
      real, intent(in), EXTEND(0) :: Image(-N:N,-N:N)
      real, intent(in), NON_ELEMENTAL :: F(-N:N,-N:N)
      integer, intent(in) :: N

      S = sum(F*Image)

   end subroutine convolve
