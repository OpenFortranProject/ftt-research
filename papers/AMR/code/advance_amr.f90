module Convolve_AMR
   integer, parameter :: N = NCELLS  ! number of cells
   integer, parameter :: H = 2       ! size of halo

   ! Number of rows in processor topology
   ! This should be a command argument somehow
   integer, parameter :: PROC_TILE = 65536
   integer, parameter :: GPU_TILE = 128

   real, allocatable, HALO(:) :: H(:) [*]
   real, allocatable, HALO(:) :: U(:) [*]
   real, allocatable, HALO(:) :: V(:) [*]
   integer, allocatable, HALO(:) :: i(:) [*]
   integer, allocatable, HALO(:) :: j(:) [*]
   integer, allocatable, HALO(:) :: level(:) [*]
   
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
      allocate( H(NCELLS)[*] HALO(2*PROC_TILE+2) )
      allocate( STENCIL(-1,1) )

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

pure CONCURRENT subroutine convolve(Image, Filter, indices)

   ! Note to Bob: I'm uncertain about this syntax.  Here user_defined_function
   ! takes the same arguments as the parent function, convolve.
   !
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
