Module MultiGrid
!
! old grid with HALO(-1:*:1), DIMENSION(0:N)
!
!  x |          o          | x    N/4-1 interior  N/4+1  total  [N=8]
!  x |    o     o     o    | x    N/2-1 interior  N/2+1  total  [N=8]
!  x | o  o  o  o  o  o  o | x    N-1   interior  N  +1  total  [N=8]
!
!    |       x| ^ |x      ???       N interior N+2 total for one parallel node (boundary shared)
!         proc boundary   ???       proc boundary is shared point
!

! new grid with HALO(-2:*:2), DIMENSION(-1:N+1)
!
! N-1, N/2-1,N/4-1,... interior cells, N+3,N/2+3,N/4+3,... total cells
!
!  x           x |          o          | o |          o          | x           x
!        x     x |    o     o     o    | o |    o     o     o    | x     x
!           x  x | o  o  o  o  o  o  o | o | o  o  o  o  o  o  o | x  x
!  
!  N=8, NP=2
!
!  N-1 interior, 4 halo cells with N+3 total points
!    - one cell on each boundary shared with other processors
!    - one halo cell at each end
!    - could treat first border cell as interior for   
!
  
  real, parameter :: PI = 4.0d0*atan(1.0d0)

Contains

Subroutine AddFourierMode(N, np, V, k)
!
! Add Fourier mode k to V
!
  implicit none
  integer, intent(in   )  ::  N, k, np
  real,    intent(inout)  ::  V(-1:N+1)  ! includes boundaries at -1,0:N,N+1
  integer :: i

  do i = -1, N+1  ! Still need to fix indexing. IMPORTANT!! (What to do about -)
    V(i) = V(i) + sin(i*k*PI/N/np)
  end do

End Subroutine AddFourierMode

Subroutine Prolongate(N, V1h, V2h)
!
!  Interpolation (prolongation) operator R^(N/2+1) => R^(N+1)
!
!  N-1 is the number of interior fine grid cells
!
  implicit none
  integer, intent(in) :: N
  real :: V1h(-1:N+1), V2h(-1:N/2+1)
  integer :: i, ii, m

  m = N/2 - 1     ! # interior coarse cells

  do i = 0, m
     ii = 2*i
     V1h(ii)   = V2h(i)
     V1h(ii+1) = 0.5*(V2h(i) + V2h(i+1))
  end do
  V1h(N) = V2h(m+1)      ! right halo cell

End Subroutine Prolongate

Subroutine Restrict(N, V1h, V2h)
!
!  Restriction operator R^(N+1) => R^(N/2+1)
!
!  N-1 is the number of interior fine grid cells
!
  implicit none
  integer, intent(in) :: N
  real :: V1h(-1:N+1), V2h(-1:N/2+1)
  integer :: i, ii, m

  m = N/2 - 1     ! # interior coarse cells

  do i = 0, m+1
     ii = 2*i
     V2h(i) = 0.25*(V1h(ii-1) + 2.0*V1h(ii) + V1h(ii+1))
  end do

End Subroutine Restrict

End Module MultiGrid
