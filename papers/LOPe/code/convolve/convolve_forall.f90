module convolve_forall

   integer, parameter :: N = 3  ! number of rows    in image
   integer, parameter :: M = 3  ! number of columns in image
   integer, parameter :: H = 1   ! size of halo

   ! Number of rows in processor topology
   ! This should be a command line argument somehow
   integer, parameter :: PROC_ROWS = 4

!$OFP real, allocatable, HALO(:,:) :: Image(:,:) [PROC_ROWS,*]
      real, allocatable            :: Image(:,:)
      real, allocatable            :: Filter(:,:)

contains


!===========================================================================
! Description: Initializes the allocatable arrays.
!
! Note about semantics of LOPe: Would like to require all array variables
! to be used in a concurrent procedure to be allocatable.  This allows
! allocation for OpenCL buffers to be done at the same time.
!
! Note that the image is a coarray and the halo is allocated as part of the
! array.  The halo is associated with the array but is not necessarily
! made contiguous with the array proper.  The decision of how to lay out the
! halo should be up to the compiler.  For example, all of the halo elements
! may be placed in contiguous memory separate from the array for more
! efficient memory copies of the halo.  The halo is assumed to be involved
! in frequent communication between coarray images and between the OpenCL
! host and device.
!
!===========================================================================

   subroutine initialize
!$OFP allocate( Image(N,M)[PROC_ROWS,*] HALO(H:H,H:H) )
      allocate( Image(1-H:N+H,1-H:M+H) )
      allocate( Filter(-H:H,-H:H) )

      ! initialize filter and image arrays
      !
      Image  = 1.0
      Filter = 2*1.0 / ((1+2*H)*(1+2*H))

   end subroutine initialize

   subroutine output_image
      integer :: row
      do row = 1-H, N+H
         print *, Image(row,:)
      end do
   end subroutine


!===========================================================================
!
! Kernel function that performs the convolution for the array element
! specified by indices.
!
!===========================================================================

!$OFP pure CONCURRENT subroutine convolve(Image, Filter, indices)
      pure            subroutine convolve(Image, Filter, indices)

!$OFP real, intent(in out), HALO(:,:) :: Image(:,:)
      real, intent(in out)            :: Image(1-H:,1-H:)
      real, intent(in)                :: Filter(-H:H,-H:H)
      integer, intent(in)             :: indices(2)

      ! local variables
      integer :: i, j
      i = indices(1)
      j = indices(2)

      Image(i,j) = sum(Filter * Image(i-H:i+H,j-H:j+H))

   end subroutine convolve

   pure real function convolve_func(Image, Filter, indices)
      real, intent(in)     :: Image(1-H:,1-H:)
      real, intent(in)     :: Filter(-H:H,-H:H)
      integer, intent(in)  :: indices(2)

      ! local variables
      integer :: i, j
      i = indices(1)
      j = indices(2)

      convolve_func = sum(Filter * Image(i-H:i+H,j-H:j+H))

   end function convolve_func

end module convolve_forall


program test_convolve
  use :: convolve_forall
  integer :: i, j

  ! temporaries handled by compiler for CONCURRENT procedure semantics
  real :: Tmp(N,M)

  call initialize

  call output_image
  print *

!$OFP do concurrent (i=1:N, j=1:M)
  forall (i=1:N, j=1:M)

!$OFP call convolve(Image, Filter)
      Tmp(i,j) = convolve_func(Image, Filter, [i,j])

!$OFP end do
  end forall

  ! temporaries handled by compiler for CONCURRENT procedure semantics
  Image(1:N,1:M) = Tmp

  call output_image

end program test_convolve
