!
! computes a convolution of V = w over A
!
! We use a local orientation
!

!
! nHalf is half of odd filter size
!
subroutine filter_1d(V, A, w, nHalf)
   use :: ForOpenCL
   implicit none
   real, dimension(:) :: V, A
   integer :: nHalf, halo(2)
   real :: A_local(-nHalf:nHalf), w(-nHalf:nHalf)

   halo = nHalf
   A_local = local_region(A, halo, halo)
   local(V) = sum(w*A_local)

end subroutine filter_1d

subroutine filter_2d(V, A, w, nPad)
   use :: ForOpenCL
   implicit none
   real, dimension(:,:) :: V, A
   integer :: nPad, halo(4)
   real :: A_local(-nPad:nPad,-nPad:nPad), w(-nPad:nPad,-nPad:nPad)

   halo = nPad
   A_local = local_region(A, halo, halo)
   local(V) = sum(w*A_local)

end subroutine filter_2d

!
! A's dimensions are scaled (expanded) relative to V
! nHalf is half of odd filter size (A's dimension)
!
subroutine filter_with_expand_1d(V, A, w, nHalf, scale)
   use :: ForOpenCL
   implicit none
   real, dimension(:) :: V, A
   integer :: nHalf, scale, halo(2)
   real :: A_local(-nHalf:nHalf), w(-nHalf:nHalf)

   halo = nHalf
   A_local = local_region(A, halo, halo, scale)
   local(V) = sum(w*A_local)

end subroutine filter_with_expand_1d
