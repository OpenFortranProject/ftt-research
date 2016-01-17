#define NO_OPENCL

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
  
  use mpi

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

Pure Subroutine RelaxBoundary_3D(N, M, L, A)
  Implicit None
  Integer, intent(in   ) :: N, M, L
  Real,    intent(inout) :: A (-1:N+1,-1:M+1,-1:L+1)
  Integer                :: i, j, k

  Real, parameter        :: w = 2.0/3.0

  !! Compute over shared boundary cells (planes) only
  !

  ! left and right planes
  do k = 1, L-1
     do j = 1, M-1
        A(0,j,k) =   (1.0 - w)  * A(0  ,j  ,k  )                        &
                   + (1.0/6.0)*w*(A(0-1,j  ,k  ) + A(0+1,j  ,k  )       &
                   +              A(0  ,j-1,k  ) + A(0  ,j+1,k  )       &
                   +              A(0  ,j  ,k-1) + A(0  ,j  ,k+1))
        A(N,j,k) =   (1.0 - w)  * A(N  ,j  ,k  )                        &
                   + (1.0/6.0)*w*(A(N-1,j  ,k  ) + A(N+1,j  ,k  )       &
                   +              A(N  ,j-1,k  ) + A(N  ,j+1,k  )       &
                   +              A(N  ,j  ,k-1) + A(N  ,j  ,k+1))
     end do
  end do

  ! down and up planes
  do k = 0, L
     do i = 0, N
        A(i,0,k) =   (1.0 - w)  * A(i  ,0  ,k  )                        &
                   + (1.0/6.0)*w*(A(i-1,0  ,k  ) + A(i+1,0  ,k  )       &
                   +              A(i  ,0-1,k  ) + A(i  ,0+1,k  )       &
                   +              A(i  ,0  ,k-1) + A(i  ,0  ,k+1))
        A(i,M,k) =   (1.0 - w)  * A(i  ,M  ,k  )                        &
                   + (1.0/6.0)*w*(A(i-1,M  ,k  ) + A(i+1,M  ,k  )       &
                   +              A(i  ,M-1,k  ) + A(i  ,M+1,k  )       &
                   +              A(i  ,M  ,k-1) + A(i  ,M  ,k+1))
    end do
 end do

  ! front and back planes
  do j = 0, M
     do i = 0, N
        A(i,j,0) =   (1.0 - w)  * A(i  ,j  ,0  )                        &
                   + (1.0/6.0)*w*(A(i-1,j  ,0  ) + A(i+1,j  ,0  )       &
                   +              A(i  ,j-1,0  ) + A(i  ,j+1,0  )       &
                   +              A(i  ,j  ,0-1) + A(i  ,j  ,0+1))
        A(i,j,L) =   (1.0 - w)  * A(i  ,j  ,L  )                        &
                   + (1.0/6.0)*w*(A(i-1,j  ,L  ) + A(i+1,j  ,L  )       &
                   +              A(i  ,j-1,L  ) + A(i  ,j+1,L  )       &
                   +              A(i  ,j  ,L-1) + A(i  ,j  ,L+1))
    end do
 end do

End Subroutine RelaxBoundary_3D

#ifdef NO_OPENCL
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
#endif

#ifdef NO_OPENCL
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
#endif

#ifdef NO_OPENCL
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
#endif

#ifdef NO_OPENCL
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
#endif

#ifdef NO_OPENCL
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
#endif

#ifdef NO_OPENCL
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
#endif

Subroutine Exchange_Halo_3D(nx, ny, nz, Array, SendBuf, RecvBuf)
!
! Exchange halo information between neighboring processes
!   - first interior plane (i=   1) sent to far halo plane of  left neighbor (i=nx+1)
!   -  last interior plane (i=nx-1) sent to far halo plane of right neighbor (i=   1)
!
   Use Parallel
   Implicit None
   Integer, intent(in   ) :: nx, ny, nz
   Real,    intent(inout) :: Array(-1:nx+1,-1:ny+1,-1:nz+1)
   Real,    intent(  out) :: SendBuf(*), RecvBuf(*)
   !----- locals -----
   Integer :: ex, ey, ez   ! ending indices of interior region (start is 1)
   Integer :: n, ierror
   Integer, Dimension(MPI_STATUS_SIZE) :: status

   ex = nx - 1
   ey = ny - 1
   ez = nz - 1

   !... X-direction
   !---------------
   n = ey * ez
   Sendbuf(1:n) = RESHAPE(source=Array(ex,1:ey,1:ez),shape=[n])

   Call MPI_SENDRECV (SendBuf, n, MPI_REAL,  Right, 1, &
                    & RecvBuf, n, MPI_REAL,   Left, 1, &
                    & MPI_COMM_CART, status, ierror)

   Array(-1,1:ey,1:ez) = RESHAPE(source=RecvBuf(1:n),shape=[ey,ez])

   !... X+direction
   !---------------
   n  = ey * ez
   Sendbuf(1:n) = RESHAPE(source=Array(1,1:ey,1:ez),shape=[n])

   Call MPI_SENDRECV (SendBuf, n, MPI_REAL,   Left, 2, &
                    & RecvBuf, n, MPI_REAL,  Right, 2, &
                    & MPI_COMM_CART, status, ierror)

   Array(nx+1,1:ey,1:ez) = RESHAPE(source=RecvBuf(1:n),shape=[ey,ez])

   !... Y-direction
   !---------------
   n = ex * ez
   Sendbuf(1:n) = RESHAPE(source=Array(1:ex,ey,1:ez),shape=[n])

   Call MPI_SENDRECV (SendBuf, n, MPI_REAL,    Top, 3, &
                    & RecvBuf, n, MPI_REAL, Bottom, 3, &
                    & MPI_COMM_CART, status, ierror)

   Array(1:ex,-1,1:ez) = RESHAPE(source=RecvBuf(1:n),shape=[ex,ez])

   !... Y+direction
   !---------------
   n = ex * ez
   Sendbuf(1:n) = RESHAPE(source=Array(1:ex,1,1:ez),shape=[n])

   Call MPI_SENDRECV (SendBuf, n, MPI_REAL, Bottom, 4, &
                    & RecvBuf, n, MPI_REAL,    Top, 4, &
                    & MPI_COMM_CART, status, ierror)

   Array(1:ex,ny+1,1:ez) = RESHAPE(source=RecvBuf(1:n),shape=[ex,ez])

   !... Z-direction
   !---------------
   n = ex * ey
   Sendbuf(1:n) = RESHAPE(source=Array(1:ex,1:ey,ez),shape=[n])

   Call MPI_SENDRECV (SendBuf, n, MPI_REAL,   Back, 5, &
                    & RecvBuf, n, MPI_REAL,  Front, 5, &
                    & MPI_COMM_CART, status, ierror)

   Array(1:ex,1:ey,-1) = RESHAPE(source=RecvBuf(1:n),shape=[ex,ey])

   !... Z+direction
   !---------------
   n = ex * ey
   Sendbuf(1:n) = RESHAPE(source=Array(1:ex,1:ey,1),shape=[n])

   Call MPI_SENDRECV (SendBuf, n, MPI_REAL,  Front, 6, &
                    & RecvBuf, n, MPI_REAL,   Back, 6, &
                    & MPI_COMM_CART, status, ierror)

   Array(1:ex,1:ey,nz+1) = RESHAPE(source=RecvBuf(1:n),shape=[ex,ey])

End Subroutine Exchange_Halo_3D

Subroutine Copyto_Halo_Buf_3D(N, M, L, Array, Buf)
! put boundaries to device, indices (0,N) ...
!
  Implicit None
  Integer, intent(in ) :: N, M, L
  Real,    intent(in ) :: Array(-1:N+1,-1:M+1,-1:L+1)
  Real,    intent(out) :: Buf(*)
  !----- locals -----
  Integer :: o, ex, ey, ez, nn

  ex = N-1
  ey = M-1
  ez = L-1

  o = 0
  nn = ey*ez
  Buf(o+1:o+nn) = RESHAPE(source=Array(0,1:ey,1:ez),shape=[nn])
  o = o + nn
  Buf(o+1:o+nn) = RESHAPE(source=Array(N,1:ey,1:ez),shape=[nn])
  o = o + nn

  nn = ex*ez
  Buf(o+1:o+nn) = RESHAPE(source=Array(1:ex,0,1:ez),shape=[nn])
  o = o + nn
  Buf(o+1:o+nn) = RESHAPE(source=Array(1:ex,M,1:ez),shape=[nn])
  o = o + nn

  nn = ex*ey
  Buf(o+1:o+nn) = RESHAPE(source=Array(1:ex,1:ey,0),shape=[nn])
  o = o + nn
  Buf(o+1:o+nn) = RESHAPE(source=Array(1:ex,1:ey,L),shape=[nn])

End Subroutine Copyto_Halo_Buf_3D

Subroutine Copyfrom_Halo_Buf_3D(N, M, L, Array, Buf)
  Implicit None
  Integer, intent(in ) :: N, M, L
  Real,    intent(out) :: Array(-1:N+1,-1:M+1,-1:L+1)
  Real,    intent(in ) :: Buf(*)
  !----- locals -----
  Integer :: o, ex, ey, ez, nn

  ex = N-1
  ey = M-1
  ez = L-1

  o = 0
  nn = ey*ez
  Array(1 ,1:ey,1:ez) = RESHAPE(source=Buf(o+1:o+nn),shape=[ey,ez])
  o = o + nn
  Array(ex,1:ey,1:ez) = RESHAPE(source=Buf(o+1:o+nn),shape=[ey,ez])
  o = o + nn

  nn = ex*ez
  Array(1:ex, 1,1:ez) = RESHAPE(source=Buf(o+1:o+nn),shape=[ex,ez])
  o = o + nn
  Array(1:ex,ey,1:ez) = RESHAPE(source=Buf(o+1:o+nn),shape=[ex,ez])
  o = o + nn

  nn = ex*ey
  Array(1:ex,1:ey, 1) = RESHAPE(source=Buf(o+1:o+nn),shape=[ex,ey])
  o = o + nn
  Array(1:ex,1:ey,ez) = RESHAPE(source=Buf(o+1:o+nn),shape=[ex,ey])

End Subroutine Copyfrom_Halo_Buf_3D

End Module MultiGrid
