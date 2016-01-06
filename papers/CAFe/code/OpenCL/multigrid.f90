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

Subroutine AddFourierMode_1D(N, V, k)
!
! Add Fourier mode k to V
!
  implicit none
  integer, intent(in   )  ::  N, k
  real,    intent(inout)  ::  V(-1:N+1)  ! includes boundaries at -1,0:N,N+1
  integer :: i

  do i = -1, N+1
    V(i) = V(i) + sin(i*k*PI/N)
  end do

End Subroutine AddFourierMode_1D

Subroutine AddFourierMode_3D(N,M,L, V, mode)
!
! Add Fourier mode k to V
!
  implicit none
  integer, intent(in   )  ::  N,M,L, mode
  real,    intent(inout)  ::  V(-1:N+1,-1:M+1,-1:L+1)  ! includes boundaries at -1,0:N,N+1 ...
  integer :: i, j, k

  !! 1D case
  !
  do k = -1, L+1
    do j = -1, M+1
      do i = -1, N+1
        V(i,j,k) = V(i,j,k) + sin(i*mode*PI/N)
      end do
    end do
  end do

End Subroutine AddFourierMode_3D

Subroutine Prolongate_1D(N, V1h, V2h)
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

End Subroutine Prolongate_1D

Subroutine Prolongate_2D(N, V1h, V2h)
!
!  Interpolation (prolongation) operator R^(N/2+1) => R^(N+1)
!
!  N-1 is the number of interior fine grid cells
!
  implicit none
  integer, intent(in) :: N
  real :: V1h(-1:N+1,-1:N+1), V2h(-1:N/2+1,-1:N/2+1)
  integer :: i, j, ii, jj, m

  m = N/2 - 1     ! # interior coarse cells

  do j = 0, m
    jj = 2*j
    do i = 0, m
      ii = 2*i

      V1h(ii,jj) = V2h(i,j)

      V1h(ii+1,jj  ) =  .5*(V2h(i,j) + V2h(i+1,j  ))
      V1h(ii  ,jj+1) =  .5*(V2h(i,j) + V2h(i  ,j+1))

      V1h(ii+1,jj+1) = .25*(V2h(i,j) + V2h(i,j+1) + V2h(i+1,j) + V2h(i+1,j+1))
    end do
  end do

  do j = 0, m
     V1h(N,j) = V2h(m+1,j)      ! right halo cells
  end do
  do i = 0, m
     V1h(i,N) = V2h(i,m+1)      ! top   halo cells
  end do

End Subroutine Prolongate_2D

Subroutine Prolongate_3D(N, V1h, V2h)
!
!  Interpolation (prolongation) operator R^(N/2+1) => R^(N+1)
!
!  N-1 is the number of interior fine grid cells
!
  implicit none
  integer, intent(in) :: N
  real :: V1h(-1:N+1,-1:N+1,-1:N+1), V2h(-1:N/2+1,-1:N/2+1,-1:N/2+1)
  integer :: i, j, k, ii, jj, kk, m

  m = N/2 - 1     ! # interior coarse cells

  do k = 0, m
    kk = 2*k
    do j = 0, m
      jj = 2*j
      do i = 0, m
        ii = 2*i

        V1h(ii,jj,kk) = V2h(i,j,k)

        V1h(ii+1,jj  ,kk  ) =   .5*(V2h(i,j,k) + V2h(i+1,j  ,k  ))
        V1h(ii  ,jj+1,kk  ) =   .5*(V2h(i,j,k) + V2h(i  ,j+1,k  ))
        V1h(ii  ,jj  ,kk+1) =   .5*(V2h(i,j,k) + V2h(i  ,j  ,k+1))

        V1h(ii+1,jj+1,kk  ) =  .25*(V2h(i,j,k) + V2h(i+1,j  ,k  )                      &
                                               + V2h(i  ,j+1,k  ) + V2h(i+1,j+1,k  ))
        V1h(ii+1,jj  ,kk+1) =  .25*(V2h(i,j,k) + V2h(i+1,j  ,k  )                      &
                                               + V2h(i  ,j  ,k+1) + V2h(i+1,j  ,k+1))
        V1h(ii  ,jj+1,kk+1) =  .25*(V2h(i,j,k) + V2h(i  ,j+1,k  )                      &
                                               + V2h(i  ,j  ,k+1) + V2h(i  ,j+1,k+1))

        V1h(ii+1,jj+1,kk+1) = .125*(V2h(i,j,k) + V2h(i+1,j  ,k  ) + V2h(i  ,j+1,k  )   &
                                                                  + V2h(i  ,j  ,k+1)   &
                                               + V2h(i+1,j+1,k  ) + V2h(i+1,j  ,k+1)   &
                                                                  + V2h(i  ,j+1,k+1)   &
                                                                  + V2h(i+1,j+1,k+1))
      end do
    end do
  end do

  do k = 0, m
    do j = 0, m
      V1h(N,j,k) = V2h(m+1,j,k)      ! right halo plane
    end do
  end do

  do k = 0, m
    do i = 0, m
      V1h(i,N,k) = V2h(i,m+1,k)      ! top   halo plane
    end do
  end do

  do j = 0, m
    do i = 0, m
      V1h(i,j,N) = V2h(i,j,m+1)      ! back  halo plane
    end do
  end do

End Subroutine Prolongate_3D

Subroutine Restrict_1D(N, V1h, V2h)
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

End Subroutine Restrict_1D

Subroutine Restrict_2D(N, V1h, V2h)
!
!  Restriction operator R^(N+1) => R^(N/2+1)
!
!  N-1 is the number of interior fine grid cells
!
  implicit none
  integer, intent(in) :: N
  real :: V1h(-1:N+1,-1:N+1), V2h(-1:N/2+1,-1:N/2+1)
  integer :: i, j, ii, jj, m

  m = N/2 - 1     ! # interior coarse cells

  do j = 0, m+1
    jj = 2*j
    do i = 0, m+1
      ii = 2*i
      V2h(i,j) = .25*(.25*V1h(ii-1,jj+1) + .5*V1h(ii,jj+1) + .25*V1h(ii+1,jj+1)   &
                    +  .5*V1h(ii-1,jj  ) +    V1h(ii,jj  ) +  .5*V1h(ii+1,jj  )   &
                    + .25*V1h(ii-1,jj-1) + .5*V1h(ii,jj-1) + .25*V1h(ii+1,jj-1))
    end do
  end do

End Subroutine Restrict_2D

Subroutine Restrict_3D(N, V1h, V2h)
!
!  Restriction operator R^(N+1) => R^(N/2+1)
!
!  N-1 is the number of interior fine grid cells
!
  implicit none
  integer, intent(in) :: N
  real :: V1h(-1:N+1,-1:N+1,-1:N+1), V2h(-1:N/2+1,-1:N/2+1,-1:N/2+1)
  integer :: i, j, k, ii, jj, kk, m

  m = N/2 - 1     ! # interior coarse cells

  do k = 0, m+1
    kk = 2*k
    do j = 0, m+1
      jj = 2*j
      do i = 0, m+1
        ii = 2*i
!!!!!   V2h(i,j,k) = .??*(
        V2h(i,j,k) = .001*(                                                              &
             .125*V1h(ii-1,jj+1,kk+1) + .25*V1h(ii,jj+1,kk+1) + .125*V1h(ii+1,jj+1,kk+1) &
           +  .25*V1h(ii-1,jj  ,kk+1) +  .5*V1h(ii,jj  ,kk+1) +  .25*V1h(ii+1,jj  ,kk+1) &
           + .125*V1h(ii-1,jj-1,kk+1) + .25*V1h(ii,jj-1,kk+1) + .125*V1h(ii+1,jj-1,kk+1) &
           +  .25*V1h(ii-1,jj+1,kk  ) +  .5*V1h(ii,jj+1,kk  ) +  .25*V1h(ii+1,jj+1,kk  ) &
           +   .5*V1h(ii-1,jj  ,kk  ) +     V1h(ii,jj  ,kk  ) +   .5*V1h(ii+1,jj  ,kk  ) &
           +  .25*V1h(ii-1,jj-1,kk  ) +  .5*V1h(ii,jj-1,kk  ) +  .25*V1h(ii+1,jj-1,kk  ) &
           + .125*V1h(ii-1,jj+1,kk-1) + .25*V1h(ii,jj+1,kk-1) + .125*V1h(ii+1,jj+1,kk-1) &
           +  .25*V1h(ii-1,jj  ,kk-1) +  .5*V1h(ii,jj  ,kk-1) +  .25*V1h(ii+1,jj  ,kk-1) &
           + .125*V1h(ii-1,jj-1,kk-1) + .25*V1h(ii,jj-1,kk-1) + .125*V1h(ii+1,jj-1,kk-1))
      end do
    end do
  end do

End Subroutine Restrict_3D


End Module MultiGrid
