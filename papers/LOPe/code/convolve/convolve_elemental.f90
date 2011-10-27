module Filter

   integer, parameter :: N

   type :: FilterPatch
      real :: p(-N:N,-N:N)
   end type FilterPatch

contains

   pure elemental subroutine convolve(SmoothedImage, Image, Filter)
      real, intent(out) :: SmoothedImage
      real, intent(in), EXTEND :: Image(-N:N,-N:N)
      type(FilterPatch), intent(in) :: Filter

      SmoothedImage = sum(Filter%p * Image)

   end subroutine convolve


end module Filter
