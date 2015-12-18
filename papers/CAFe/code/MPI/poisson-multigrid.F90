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
Integer :: nsteps = 10

Real, allocatable :: V1h(:), Tmp(:), V2h(:), V4h(:), V8h(:)

Allocate(V1h(-1:N+1), Tmp(-1:N+1))
Allocate(V2h(-1:N/2+1))
Allocate(V4h(-1:N/4+1))
Allocate(V8h(-1:N/8+1))

open(unit=fd, file="error_time.dat")

!! Initialize
!
V1h = 0.0
Call AddFourierMode(N, V1h,  1)
Call AddFourierMode(N, V1h,  6)
Call AddFourierMode(N, V1h, 16)
V1h = (1./3.)*V1h

!... Relax solution on 1h mesh
!    -------------------------
Tmp = 0.0
Call Textual_Output(N, V1h, "1h_0")
do i = 2, nsteps, 2
  Call Relax(N, V1h, Tmp)      ; Call Exchange_Halo(N, Tmp)
  Call Relax(N, Tmp, V1h)      ; Call Exchange_Halo(N, V1h)
  write(fd, *) i, maxval(V1h)
end do
Call Textual_Output(N, V1h, "1h_mid")

!... Relax solution on 2h mesh
!    -------------------------
Tmp = 0.0
Call Restrict(N, V1h, V2h)
Call Textual_Output(N/2, V2h, "2h_0")
do i = 2, nsteps, 2
  Call Relax(N/2, V2h, Tmp)      ; Call Exchange_Halo(N/2, Tmp)
  Call Relax(N/2, Tmp, V2h)      ; Call Exchange_Halo(N/2, V2h)
  write(fd, *) i, maxval(V2h)
end do
Call Textual_Output(N/2, V2h, "2h_mid")

!... Relax solution on 4h mesh
!    -------------------------
Tmp = 0.0
Call Restrict(N/2, V2h, V4h)
Call Textual_Output(N/4, V4h, "4h_0")
do i = 2, nsteps, 2
  Call Relax(N/4, V4h, Tmp)      ; Call Exchange_Halo(N/4, Tmp)
  Call Relax(N/4, Tmp, V4h)      ; Call Exchange_Halo(N/4, V4h)
  write(fd, *) i, maxval(V4h)
end do
Call Textual_Output(N/4, V4h, "4h_mid")

!... Relax solution on 8h mesh
!    -------------------------
Tmp = 0.0
Call Restrict(N/4, V4h, V8h)
Call Textual_Output(N/8, V8h, "8h_0")
do i = 2, nsteps, 2
  Call Relax(N/8, V8h, Tmp)      ; Call Exchange_Halo(N/8, Tmp)
  Call Relax(N/8, Tmp, V8h)      ; Call Exchange_Halo(N/8, V8h)
  write(fd, *) i, maxval(V8h)
end do
Call Textual_Output(N/8, V8h, "8h_mid")

!! IMPORTANT: this last step should be an exact solution on a smaller grid probably
!
do i = 2, 5*nsteps, 2
  Call Relax(N/8, V8h, Tmp)      ; Call Exchange_Halo(N/8, Tmp)
  Call Relax(N/8, Tmp, V8h)      ; Call Exchange_Halo(N/8, V8h)
  write(fd, *) i, maxval(V8h)
end do
Call Textual_Output(N/8, V8h, "8h_end")

!! IMPORTANT: probably should relax on the way back down the grids as well
!
Call Prolongate    (N/4, V4h, V8h)
Call Textual_Output(N/4, V4h, "4h_end")

Call Prolongate    (N/2, V2h, V4h)
Call Textual_Output(N/2, V2h, "2h_end")

Call Prolongate    (N,   V1h, V2h)
Call Textual_Output(N,   V1h, "1h_end")

close(fd)

CONTAINS

Pure Subroutine Relax(N, A, Adv)
!
! Relax on the interior and the two halo cells shared with the left and right neighbors
!   - shared halo cells are computed twice and are not exchanged
!   - the outside halo cells are from neighbors and cannot be not computed
!
   Implicit None
   Integer, intent( in) :: N
   Real,    intent( in) :: A  (-1:N+1)
   Real,    intent(out) :: Adv(-1:N+1)
   Integer              :: i

   do i = 0, N
      Adv(i) = (1.0 - w)*A(i) + 0.5*w*(A(i-1) + A(i+1))
   end do

End Subroutine Relax

Subroutine Exchange_Halo(N, A)
!
! Exchange halo information between neighboring processes
!
   Implicit None
   Integer, intent( in) :: N
   Real,    intent( in) :: A(-1:N+1)
End Subroutine Exchange_Halo

End Program PoissonMultigrid
