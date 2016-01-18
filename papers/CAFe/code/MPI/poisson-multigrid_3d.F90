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
Use mpi_f08
Use MultiGrid, only : AddFourierMode_3d
Use IO,        only : Textual_Output
Use MultiGrid, only : Restrict, Prolongate

Implicit None

Real, parameter :: w = (2.0/3.0)

Integer, parameter :: N      =  64
Integer, parameter :: fd     =  12

Integer :: i
Integer :: nsteps = 10 

Integer :: rank, np

Real, allocatable :: V1h(:), Tmp(:), V2h(:), V4h(:), V8h(:)

Call MPI_Init()
Call MPI_Comm_size( MPI_COMM_WORLD, np)
Call MPI_Comm_rank( MPI_COMM_WORLD, rank)

Allocate(V1h(-1:N+1), Tmp(-1:N+1))
Allocate(V2h(-1:N/2+1))
Allocate(V4h(-1:N/4+1))
Allocate(V8h(-1:N/8+1))

!if (rank == 0) then
  open(unit=fd, file="error_time.dat")
!end if

!! Initialize
!
print ("('[', i2, ']:', 1x, 'size:', i3)"), rank, np

!
V1h = 0.0
Call AddFourierMode_3d(N, np, rank, V1h,  1)
Call AddFourierMode_3d(N, np, rank, V1h,  6)
Call AddFourierMode_3d(N, np, rank, V1h, 16)
V1h = (1./3.)*V1h

!... Relax solution on 1h mesh
!    -------------------------
Call Textual_Output(rank, N, V1h, "1h_0")
do i = 1, nsteps
  Call Relax(rank, np, N, V1h, Tmp)
  Call Exchange_Halo(rank, N, V1h)
  write(fd, *) i, maxval(V1h)
end do
Call Textual_Output(rank, N, V1h, "1h_mid")

!... Relax solution on 2h mesh
!    -------------------------
Call Restrict(N, V1h, V2h)
Call Textual_Output(rank, N/2, V2h, "2h_0")
do i = 1, nsteps
  Call Relax(rank, np, N/2, V2h, Tmp)
  Call Exchange_Halo(rank, N/2, V2h)
  write(fd, *) i, maxval(V2h)
end do
Call Textual_Output(rank, N/2, V2h, "2h_mid")

!... Relax solution on 4h mesh
!    -------------------------
Call Restrict(N/2, V2h, V4h)
Call Textual_Output(rank, N/4, V4h, "4h_0")
do i = 1, nsteps
  Call Relax(rank, np, N/4, V4h, Tmp)
  Call Exchange_Halo(rank, N/4, V4h)
  write(fd, *) i, maxval(V4h)
end do
Call Textual_Output(rank, N/4, V4h, "4h_mid")

!... Relax solution on 8h mesh
!    -------------------------
Call Restrict(N/4, V4h, V8h)
Call Textual_Output(rank, N/8, V8h, "8h_0")
do i = 1, nsteps
  Call Relax(rank, np, N/8, V8h, Tmp)
  Call Exchange_Halo(rank, N/8, V8h)
  write(fd, *) i, maxval(V8h)
end do
Call Textual_Output(rank, N/8, V8h, "8h_mid")

!! IMPORTANT: this last step should be an exact solution on a smaller grid probably
!
do i = 1, 5*nsteps
  Call Relax(rank, np, N/8, V8h, Tmp)
  Call Exchange_Halo(rank, N/8, V8h)
  write(fd, *) i, maxval(V8h)
end do
Call Textual_Output(rank, N/8, V8h, "8h_end")

!! IMPORTANT: probably should relax on the way back down the grids as well
!
Call Prolongate    (N/4, V4h, V8h)
Call Textual_Output(rank, N/4, V4h, "4h_end")

Call Prolongate    (N/2, V2h, V4h)
Call Textual_Output(rank, N/2, V2h, "2h_end")

Call Prolongate    (N,   V1h, V2h)
Call Textual_Output(rank, N,   V1h, "1h_end")

close(fd)

Call MPI_Finalize()

CONTAINS

Pure Subroutine Relax(rank, np, N, A, Tmp)
!
! Relax on the interior and the two halo cells shared with the left and right neighbors
!   - shared halo cells are computed twice and are not exchanged
!   - the outside halo cells are from neighbors and cannot be not computed
!
   Implicit None
   Integer, intent(in   ) :: N, rank, np
   Real,    intent(inout) :: A  (-1:N+1)
   Real,    intent(inout) :: Tmp(-1:N+1)
   Integer                :: i

   ! compute over extended region including boundary cells
   do i = 0, N
      Tmp(i) = (1.0 - w)*A(i) + 0.5*w*(A(i-1) + A(i+1))
   end do

   !! set physical boundary conditions (they have been recomputed)
   !    - probably should have rank information so that physical boundaries aren't changed
   !
   !! Need to synchronize here as we may be running concurrently
   !   - on subimage will only synchronize with its hardware threads, not distributed memory
   !
   ! Sync All

   ! compute over just the interior
   do i = 1, N-1
      A(i) = (1.0 - w)*Tmp(i) + 0.5*w*(Tmp(i-1) + Tmp(i+1))
   end do

   !! IMPORTANT: not sure why this is needed, perhaps an error in prolongation/restrict
   !

End Subroutine Relax

Subroutine Exchange_Halo(rank, N, A)
!
! Exchange halo information between neighboring processes
!
   Implicit None
   Integer, intent(in   ) :: N, rank
   Real,    intent(inout) :: A(-1:N+1)

   Integer :: left, right, tag
   Real    :: lbc, rbc   
   type(MPI_Status) :: status
   tag   = 1
   left  = rank - 1
   right = rank + 1

   if (left < 0)                        left  = np-1
   if (right > np-1)                    right = 0   

   !! MPI halo exchange for parallel version
   !
   Call MPI_Send(A(1), 1, MPI_REAL, left, tag, MPI_COMM_WORLD)
   Call MPI_Send(A(N-1)  , 1, MPI_REAL, right , tag, MPI_COMM_WORLD)

   Call MPI_Recv(rbc, 1, MPI_REAL, right, tag, MPI_COMM_WORLD, status)
   Call MPI_Recv(lbc, 1, MPI_REAL, left , tag, MPI_COMM_WORLD, status)
   
   A( -1) = lbc
   A(N+1) = rbc



End Subroutine Exchange_Halo

End Program PoissonMultigrid
