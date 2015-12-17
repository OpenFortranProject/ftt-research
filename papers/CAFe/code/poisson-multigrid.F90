Program PoissonMultigrid
!=============================================================================
! Name        : poisson-multigrid.F90
! Version     :
! Description : Solves the Poisson equation in one dimension.
!
! Method :
!
!   Multigrid.
!
!=============================================================================
Use MultiGrid, only : AddFourierMode
Use IO,        only : Textual_Output
!Use Jacobi,   only : Iterate, Iterate1D
!Use Jacobi,   only : Iterate1D
Use MultiGrid, only : Restrict, Prolongate

Implicit None

!Real, parameter :: w = 1.0
Real, parameter :: w = (2.0/3.0)
Logical :: first = .true.

Integer, parameter :: NDIMS  =   1
Integer, parameter :: N      =  64
Integer, parameter :: fd     =  12

Integer :: i
Integer :: nsteps  =  10

Real, allocatable :: V1h(:), Vp1h(:), V2h(:), V4h(:), V8h(:)

Allocate(V1h(N+1), Vp1h(N+1))
Allocate(V2h(N/2+1))
Allocate(V4h(N/4+1))
Allocate(V8h(N/8+1))

open(unit=fd, file="error_time.dat")

!! Initialize
!
V1h    = 0.0
Call AddFourierMode(N, V1h,  1)
Call AddFourierMode(N, V1h,  6)
Call AddFourierMode(N, V1h, 16)
V1h = (1./3.)*V1h

!... Relax solution on 1h mesh
!    -------------------------
Vp1h = 0.0
Call Textual_Output(N,   V1h, "1h_0")
do i = 2, nsteps, 2
  Call Relax(N, V1h,  Vp1h)
  Call Relax(N, Vp1h, V1h )
  write(fd, *) i, maxval(V1h)
end do
Call Textual_Output(N, V1h, "1h_mid")

!... Relax solution on 2h mesh
!    -------------------------
Vp1h = 0.0
Call Restrict(N, V1h, V2h)
Call Textual_Output(N/2, V2h, "2h_0")
do i = 2, nsteps, 2
  Call Relax(N/2, V2h,  Vp1h)
  Call Relax(N/2, Vp1h, V2h )
  write(fd, *) i, maxval(V2h)
end do
Call Textual_Output(N/2, V2h, "2h_mid")

!... Relax solution on 4h mesh
!    -------------------------
Vp1h = 0.0
Call Restrict(N/2, V2h, V4h)
Call Textual_Output(N/4, V4h, "4h_0")
do i = 2, nsteps, 2
  Call Relax(N/4, V4h,  Vp1h)
  Call Relax(N/4, Vp1h, V4h )
  write(fd, *) i, maxval(V4h)
end do
Call Textual_Output(N/4, V4h, "4h_mid")

!... Relax solution on 8h mesh
!    -------------------------
Vp1h = 0.0
Call Restrict(N/4, V4h, V8h)
Call Textual_Output(N/8, V8h, "8h_0")
do i = 2, nsteps, 2
  Call Relax(N/8, V8h,  Vp1h)
  Call Relax(N/8, Vp1h, V8h )
  write(fd, *) i, maxval(V8h)
end do
Call Textual_Output(N/8, V8h, "8h_mid")

do i = 2, 5*nsteps, 2
  Call Relax(N/8, V8h,  Vp1h)
  Call Relax(N/8, Vp1h, V8h )
  write(fd, *) i, maxval(V8h)
end do
Call Textual_Output(N/8, V8h, "8h_end")

Call Prolongate    (N/4, V4h, V8h)
Call Textual_Output(N/4, V4h, "4h_end")

Call Prolongate    (N/2, V2h, V4h)
Call Textual_Output(N/2, V2h, "2h_end")

Call Prolongate    (N,   V1h, V2h)
Call Textual_Output(N,   V1h, "1h_end")

close(fd)

CONTAINS

Pure Subroutine Relax(N, A, Adv)
   Implicit None
   Integer, intent( in) :: N
   Real,    intent( in) :: A  (0:N)
   Real,    intent(out) :: Adv(0:N)
   Integer              :: i

   do i = 1, N-1
      Adv(i) = (1.0 - w)*A(i) +  0.5*w*(A(i-1) + A(i+1))
   end do

End Subroutine Relax

End Program PoissonMultigrid
