module Convolve_LOPe

   integer, parameter :: N = 64  ! number of rows    in image
   integer, parameter :: M = 64  ! number of columns in image
   integer, parameter :: H = 1   ! size of halo

   ! Number of rows in processor topology
   ! This should be a command line argument somehow
   integer, parameter :: PROC_ROWS = 4

   ! This is syntax we'd like to have including coarray notation
   !
   ! real, allocatable, HALO(:,:) :: Image(:,:) [*,*]    ! [*,*] is new syntax

   !$OFP HALO(:,:) :: Image
   real, allocatable :: Image(:,:)
   real, allocatable :: Filter(:,:)

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
!$OFP allocate( Image(N,M)[*,*] HALO([H,H],[H,H]) )
      allocate( Image(N+2*H,M+2*H) )
      allocate( Filter(-H:H,-H:H) )

      ! initialize filter and image arrays
      !
      Image  = 1.0
      Filter = 1.0 / ((1+2*H)*(1+2*H))

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

!$OFP CONCURRENT :: convolve
pure subroutine convolve(Image, Filter)

!$OFP HALO(:,:) :: Image(0:,0:)
   real, intent(in out)  :: Image(-H:,-H:)
   real, intent(in)      :: Filter(-H:H,-H:H)

   Image(0,0) = sum(Filter * Image(-H:+H,-H:+H))

end subroutine convolve

end module Convolve_LOPe


program convolve_image
   use :: Convolve_LOPe
   integer :: i, j

   ! initialize Filter and Image coarray data on this image
   !
   call initialize

   ! call intrinsic function to exchange halo data between images
   !
   call exchange_halo(Image)

!$OFP do concurrent (i=1:N, j=1:M)
!$OFP    call convolve(Image(i,j), Filter)
   call convolve(Image, Filter)   
!$OFP end do

   ! output local data (including coarrays)
   !
   call output_image

end program convolve_image
