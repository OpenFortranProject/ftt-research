!
! A 2x2 filter applied over an array 2x2 larger than destination
!
!  V = V + sum(w*A)
!
subroutine filter_2x2(nPad, V, A, w)
  use :: ForOpenCL
  implicit none
  integer, intent(in) :: nPad
  real, dimension(:,:) :: V, A  ! V 2x2 bigger
  real, dimension(2,2) :: w

  real, dimension(:,:), allocatable :: A_big

  ! scatter A to array compatible with V
  !
  A_big = scatter(identity, A, 2)

end subroutine filter_2x2


!subroutine convolve(nPad, V, A, w)
   ! nx = get_global_size(0), ny ... is default, otherwise explicit
!   real, dimension(:,:) :: V, A, w
!   asynchronous :: A     ! block on event?

!   real, pointer, dimension(1+2*nPad,1+2*nPad) :: lA

!   halo   = [nPad,nPad,nPad,nPad]
!   filter = [nPad,nPad,nPad,nPad]

   ! A is extended
   ! iA => halo(A, aHalo)      ! k,kl indexes

!   lA => halo(A, halo, local=filter)

!   V = V + sum(w*lA)         ! this does convolution

!end subroutine convolve
