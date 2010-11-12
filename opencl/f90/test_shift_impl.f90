subroutine loops_c_f(A, B, nx, ny, nPad) bind(C,name="loops_c_f")
   use, intrinsic :: ISO_C_BINDING
   implicit none
   real(c_float), dimension(*) :: A, B
   integer(c_int), value :: nx, ny, nPad

   integer :: k, nk, sy, offset

   nk = nx*ny
   sy = nx + 2*nPad
   offset = nPad + nPad*sy

   do k = 1, nk
      A(k) = B(k)
   end do

end subroutine loops_c_f


subroutine shift_f90(A, B, nPad)
   implicit none
   real, dimension(:,:) :: A, B
   integer :: nPad
   integer :: nx, ny, ub(2)

   ub = ubound(A)
   nx = ub(1); ny = ub(2)

!   B(2:nx-1,2:ny-1) = ( A(2:nx-1,2:ny-1)                          &
!                    +   cshift(A(2:nx-1,2:ny-1), shift=-1, dim=1) &
!                    +   cshift(A(2:nx-1,2:ny-1), shift=+1, dim=1) &
!                    +   cshift(A(2:nx-1,2:ny-1), shift=-1, dim=2) &
!                    +   cshift(A(2:nx-1,2:ny-1), shift=+1, dim=2) &
!                      ) / 5.0
B(2:nx-1,2:ny-1) = A(2:nx-1,2:ny-1)
end subroutine shift_f90

subroutine loops_f90(A, B, nPad)
   implicit none
   real, dimension(:,:) :: A, B
   integer :: nPad
   integer :: i, j, nx, ny, ub(2)

   ub = ubound(A)
   nx = ub(1); ny = ub(2)

   do j = 2, ny-1
      do i = 2, nx-1
!         B(i,j) = ( A(i,j)   &
!                +   A(i-1,j) &
!                +   A(i+1,j) &
!                +   A(i,j-1) &
!                +   A(i,j+1) &
!                  ) / 5.0
    B(i,j) = A(i,j)
      end do
   end do
end subroutine loops_f90

