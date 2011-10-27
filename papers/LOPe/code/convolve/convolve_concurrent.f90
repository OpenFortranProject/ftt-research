module Filter
   integer, parameter :: N = 1
contains

   pure CONCURRENT subroutine convolve(Image, Filter, indices)
      real, intent(in out), HALO(:,:) :: Image(:,:)
      real, intent(in) :: Filter(-N:N,-N:N)
      integer :: indices(2)

      integer :: i = indices(1)
      integer :: j = indices(2)

      Image(i,j) = sum(Filter * Image(i-N:i+N,j-N:j+N))

   end subroutine convolve

end module Filter
