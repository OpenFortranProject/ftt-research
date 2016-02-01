Program PoissonMultigrid
!=============================================================================
! Name        : poisson-multigrid_1d.F90
! Version     :
! Description : Solves the Poisson equation in one dimension.
!
! Method :
!
!   Multigrid.
!
!=============================================================================
Use mpi_f08
Use MultiGrid, only : AddFourierMode_1D
Use IO,        only : Textual_Output_1D
Use MultiGrid, only : Restrict_1D, Prolongate_1D

Implicit None

Real, parameter :: w = (2.0/3.0)

Integer, parameter :: N      =  64
Integer, parameter :: fd     =  12

Integer :: i,k
Integer :: nsteps = 20

Integer :: rank, np

Real, allocatable :: V1h(:), Tmp(:), V2h(:), V4h(:), V8h(:)

Call MPI_Init()
Call MPI_Comm_size( MPI_COMM_WORLD, np)
Call MPI_Comm_rank( MPI_COMM_WORLD, rank)

Allocate(V1h(-1:N+1), Tmp(-1:N+1))
Allocate(V2h(-1:N/2+1))
Allocate(V4h(-1:N/4+1))
Allocate(V8h(-1:N/8+1))

  open(unit=fd, file="error_time.dat")

!! Initialize
!
print ("('[', i2, ']:', 1x, 'size:', i3)"), rank, np

!
V1h = 0.0
Call AddFourierMode_1D(N, np, rank, V1h,  1)
Call AddFourierMode_1D(N, np, rank, V1h,  6)
Call AddFourierMode_1D(N, np, rank, V1h, 16)
V1h = (1./3.)*V1h
Call Textual_Output_1D(rank, np, N, V1h, "1h_0")

!do k=1,8        ! Big loop
!... Relax_1D solution on 1h mesh
!    -------------------------
do i = 1, nsteps
  Call Relax_1D(N, V1h, Tmp)
  Call Exchange_Halo_1D(rank, N, V1h)
  write(fd, *) i, maxval(V1h)
end do
Call Textual_Output_1D(rank, np, N, V1h, "1h_mid")
!... Relax_1D solution on 2h mesh
!    -------------------------
Call Restrict_1D(N, V1h, V2h)
Call Textual_Output_1D(rank, np, N/2, V2h, "2h_0")
do i = 1, nsteps
  Call Relax_1D(N/2, V2h, Tmp)
  Call Exchange_Halo_1D(rank, N/2, V2h)
  write(fd, *) i, maxval(V2h)
end do
Call Textual_Output_1D(rank, np, N/2, V2h, "2h_mid")

!... Relax_1D solution on 4h mesh
!    -------------------------
Call Restrict_1D(N/2, V2h, V4h)
Call Textual_Output_1D(rank, np, N/4, V4h, "4h_0")
do i = 1, nsteps
  Call Relax_1D(N/4, V4h, Tmp)
  Call Exchange_Halo_1D(rank, N/4, V4h)
  write(fd, *) i, maxval(V4h)
end do
Call Textual_Output_1D(rank, np, N/4, V4h, "4h_mid")

!... Relax_1D solution on 8h mesh
!    -------------------------
Call Restrict_1D(N/4, V4h, V8h)
Call Textual_Output_1D(rank, np, N/8, V8h, "8h_0")
do i = 1, nsteps
  Call Relax_1D(N/8, V8h, Tmp)
  Call Exchange_Halo_1D(rank, N/8, V8h)
  write(fd, *) i, maxval(V8h)
end do
Call Textual_Output_1D(rank, np, N/8, V8h, "8h_mid")

!! IMPORTANT: this last step should be an exact solution on a smaller grid probably
!
!Set exact solution on coarsest grid.
!V8h = 0.0
do i = 1, nsteps
  Call Relax_1D(N/8, V8h, Tmp)
  Call Exchange_Halo_1D(rank, N/8, V8h)
  write(fd, *) i, maxval(V8h)
end do
Call Textual_Output_1D(rank, np, N/8, V8h, "8h_end")

!! IMPORTANT: probably should relax_1D on the way back down the grids as well
!
Call Prolongate_1D    (N/4, V4h, V8h)
do i = 1, nsteps
  Call Relax_1D(N/4, V4h, Tmp)
  Call Exchange_Halo_1D(rank, N/4, V4h)
  write(fd, *) i, maxval(V4h)
end do
Call Textual_Output_1D(rank, np, N/4, V4h, "4h_end")

Call Prolongate_1D    (N/2, V2h, V4h)
do i = 1, nsteps
  Call Relax_1D(N/2, V2h, Tmp)
  Call Exchange_Halo_1D(rank, N/2, V2h)
  write(fd, *) i, maxval(V2h)
end do
Call Textual_Output_1D(rank, np, N/2, V2h, "2h_end")

Call Prolongate_1D    (N,   V1h, V2h)
do i = 1, nsteps
  Call Relax_1D(N, V1h, Tmp)
  Call Exchange_Halo_1D(rank, N, V1h)
  write(fd, *) i, maxval(V1h)
end do
Call Textual_Output_1D(rank, np, N,   V1h, "1h_end")
!end do !! Big loop
close(fd)

Call MPI_Finalize()

CONTAINS

Subroutine Relax_1D(N, A, Tmp)
!
! Relax_1D on the interior and the two halo cells shared with the left and right neighbors
!   - shared halo cells are computed twice and are not exchanged
!   - the outside halo cells are from neighbors and cannot be not computed
!
   Implicit None
   Integer, intent(in   ) :: N
   Real,    intent(inout) :: A  (-1:N+1)
   Real,    intent(inout) :: Tmp(-1:N+1)
   Integer                :: i

   ! compute over extended region excluding the boundary cells

   do i = 1, N-1
      Tmp(i) = (1.0 - w)*A(i) + 0.5*w*(A(i-1) + A(i+1))
   end do
 
   !
   ! compute over just the interior
   do i = 1, N-1
      A(i) = (1.0 - w)*Tmp(i) + 0.5*w*(Tmp(i-1) + Tmp(i+1))
   end do


End Subroutine Relax_1D

Subroutine Exchange_Halo_1D(rank, N, A)
!
! Exchange halo information between neighboring processes
!
   Implicit None
   Integer, intent(in   ) :: N, rank
   Real,    intent(inout) :: A(-1:N+1)

   Integer :: left, right, tag
   type(MPI_Status) :: status
   tag   = 1
   left  = rank - 1
   right = rank + 1

   if (left < 0) then
   left  = np-1
   else if (right > np-1) then
   right = 0 
   end if
   
  A(1) = (1.0 - w)*A(1) + 0.5*w*(A(0) + A(2))
  A(N-1) = (1.0 - w)*A(N-1) + 0.5*w*(A(N-2) + A(N))
   !! MPI halo exchange for parallel version
   !
   Call MPI_Send(A(1)  ,  1, MPI_REAL, left , tag, MPI_COMM_WORLD)
   Call MPI_Send(A(N-1),  1, MPI_REAL, right, tag, MPI_COMM_WORLD)
   Call MPI_Recv(A(-1) ,  1, MPI_REAL, left , tag, MPI_COMM_WORLD, status)
   Call MPI_Recv(A(N+1),  1, MPI_REAL, right, tag, MPI_COMM_WORLD, status)
   

   !! Relax on the last two points now that we have exchanged halo points.

   A(0) = (1.0 - w)*A(0) + 0.5*w*(A(-1) + A(1))
   A(N) = (1.0 - w)*A(N) + 0.5*w*(A(N-1) + A(N+1))

   !! set physical boundary conditions (they have been recomputed)
   if (left  == np-1) then
   A(0) = 0.0
   end if
   if (right == 0   ) then
   A(N) = 0.0 
   end if
   
End Subroutine Exchange_Halo_1D

End Program PoissonMultigrid
