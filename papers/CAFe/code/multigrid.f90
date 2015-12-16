Module MultiGrid
!
!  x |          o          | x    N/4-1 interior  N/4+1  total  [N=8]
!  x |    o     o     o    | x    N/2-1 interior  N/2+1  total  [N=8]
!  x | o  o  o  o  o  o  o | x    N-1   interior  N  +1  total  [N=8]
!
!    |       x| ^ |x      ???       N interior N+2 total for one parallel node (boundary shared)
!         proc boundary   ???       proc boundary is shared point
!

  real, parameter :: PI = 4.0d0*atan(1.0d0)

Contains

Subroutine AddFourierMode(N, V, k)
!
! Add Fourier mode k to V
!
  implicit none
  integer, intent(in   )  ::  N, k
  real,    intent(inout)  ::  V(0:N)  ! includes boundaries at 0,N
  integer :: i

  do i = 0, N
    V(i) = V(i) + sin(i*k*PI/N)
  end do

End Subroutine AddFourierMode

Subroutine Prolongate(N, V1h, V2h)
!
!  Interpolation (prolongation) operator R^(N/2-1) => R^(N-1)
!
!  N-1 is the number of interior fine grid cells
!
  implicit none
  integer, intent(in) :: N
  real :: V1h(N+1), V2h(N/2+1)
  integer :: i, ii, m

  m = N/2 - 1     ! # interior coarse cells

  V1h(2) = 0.5*(V2h(1) + V2h(2))   ! first interior cell
  do i = 2, m+1
     ii = 2*i - 1
     V1h(ii)   = V2h(i)
     V1h(ii+1) = 0.5*(V2h(i) + V2h(i+1))
  end do

End Subroutine Prolongate

Subroutine Restrict(N, V1h, V2h)
!
!  Restriction operator R^(N-1) => R^(N/2-1)
!
!  N-1 is the number of interior fine grid cells
!
  implicit none
  integer, intent(in) :: N
  real :: V1h(N+1), V2h(N/2+1)
  integer :: i, ii, m

  m = N/2 - 1     ! # interior coarse cells

  do i = 2, m+1
     ii = 2*i - 1
     V2h(i) = 0.25*(V1h(ii-1) + 2.0*V1h(ii) + V1h(ii+1))
  end do

End Subroutine Restrict

End Module MultiGrid
