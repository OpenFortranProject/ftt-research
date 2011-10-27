module WeightType
   use WeightParams

   type :: WeightPatch
      real :: p(-NPAD:NPAD,-NPAD:NPAD)
   end type WeightPatch

contains

   pure elemental subroutine convolve_elemental(SmoothedImage, Image, Filter)
      real, intent(out) :: SmoothedImage
      !$OFP EXTEND :: Image(-N:N,-N:N)
      real, intent(in) :: Image
      type(WeightPatch), intent(in) :: Filter

      ! SmoothedImage = sum(Filter%p * Image)
      SmoothedImage = Filter%p(1,1) * Image

   end subroutine convolve_elemental

end module WeightType
