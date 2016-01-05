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
Use MultiGrid, only : Restrict, Prolongate

Implicit None

Real, parameter :: w = (2.0/3.0)

Integer, parameter :: N      =  64
Integer, parameter :: fd     =  12
Integer, parameter :: nsteps =   5
Integer :: i, l, submiage

Real, allocatable, dimension(:), codimension[:] :: V1h, V2h, V4h, V8h, Tmp

open(unit=fd, file="error_time.dat")

subimage = get_subimage(1)

Allocate(V1h(-1:N  +1) [*])
Allocate(V2h(-1:N/2+1) [*])
Allocate(V4h(-1:N/4+1) [*])
Allocate(V8h(-1:N/8+1) [*])
Allocate(Tmp(-1:N  +1) [*])

if (subimage /= THIS_IMAGE()) then
   Allocate(V1h(-1:N  +1) [*])
   Allocate(V2h(-1:N/2+1) [*])
   Allocate(V4h(-1:N/4+1) [*])
   Allocate(V8h(-1:N/8+1) [*])
   Allocate(Tmp(-1:N  +1) [*])
endif

!... Initialize
!    ----------
V1h = 0.0
Call AddFourierMode(N, V1h,  1)
Call AddFourierMode(N, V1h,  6)
Call AddFourierMode(N, V1h, 16)
V1h = (1./3.)*V1h

!... Copy array data to subimage
!    ---------------------------
V1h[subimage] = V1h
V2h[subimage] = V2h
V4h[subimage] = V4h
V8h[subimage] = V8h
Tmp[subimage] = Tmp

!... Relax solution on 1h mesh
!    -------------------------
Do r = 1, nsteps
  Do Concurrent(i=0:N)  [[subimage]] 
     Call Relax(N, V1h(i)[subimage], Tmp(i)[subimage])
  End Do
  Call Exchange_Halo(N, V1h)
End Do

!... Relax solution on 2h mesh
!    -------------------------
Do Concurrent(i=1:N-1)   [[subimage]] 
   Call Restrict(N, V1h(i)[subimage], V2h(i)[subimage])
End Do
Do l = 1, nsteps
  Do Concurrent(i=0:N)    [[subimage]] 
     Call Relax(N/2, V2h(i)[subimage], Tmp(i)[subimage])
  End Do
  Call Exchange_Halo(N/2, V2h)
End Do

!... Relax solution on 4h mesh
!    -------------------------
Do Concurrent(i=1:N-1)     [[subimage]] 
   Call Restrict(N/2, V2h(i)[subimage], V4h(i)[subimage])
End Do
Do l = 1, nsteps
  Do Concurrent(i=0:N)    [[subimage]]
     Call Relax(N/4, V4h(i)[subimage], Tmp(i)[subimage])
  End Do
  Call Exchange_Halo(N/4, V4h)
End Do

!... Relax solution on 8h mesh
!    -------------------------
Do Concurrent(i=1:N-1)     [[subimage]] 
   Call Restrict(N/4, V4h(i)[subimage], V8h(i)[subimage])
End Do
Do l = 1, nsteps
  Do Concurrent(i=0:N)    [[subimage]] 
     Call Relax(N/8, V8h(i)[subimage], Tmp(i)[subimage])
  End Do
  Call Exchange_Halo(N/8, V8h)
End Do

!! IMPORTANT: copy data to [root] and solve
!
V8h = V8h[subimage]
Call Restrict(N/8, V8h, Tmp)
Call Solve(N/16, Tmp)
Call Prolongate(N/8, V8h, Tmp)

!! IMPORTANT: probably should relax on the way back down the grids as well
!
V8h[subimage] = V8h
Do Concurrent(i=1:N-1)       [[subimage]] 
   Call Prolongate(N/4, V4h(i)[subimage], V8h(i)[subimage])
End Do
Do Concurrent(i=1:N-1)       [[subimage]] 
   Call Prolongate(N/2, V2h(i)[subimage], V4h(i)[subimage])
End Do
Do Concurrent(i=1:N-1)       [[subimage]] 
   Call Prolongate(N,   V1h(i)[subimage], V2h(i)[subimage])
End Do

!! Finished...
!
V1h = V1h[subimage]

Close(fd)

Contains

Pure CONCURRENT Subroutine Relax(N, A, Tmp)
!
! Relax on the interior and the two halo cells shared with the left and right neighbors
!   - shared halo cells are computed twice and are not exchanged
!   - the outside halo cells are from neighbors and cannot be not computed
!
   Implicit None
   Integer, intent(in   ) :: N
   Real,    intent(inout) :: A  (-1:N+1)
   Real,    intent(inout) :: Tmp(-1:N+1)
   Integer                :: i, begin, end

   !! compute over extended region including boundary cells
   !
   if (this_image() /= 1)            begin = 0
   else                              begin = 1

   if (this_image() /= num_images()) end = N
   else                              end = N - 1

   do i = begin, end
      Tmp(i) = (1.0 - w)*A(i) + 0.5*w*(A(i-1) + A(i+1))
   end do

   !! need to synchronize here as we may be running concurrently
   !
   sync_team(this_subimage())

   !! compute over just the interior
   !
   do i = 1, N-1
      A(i) = (1.0 - w)*Tmp(i) + 0.5*w*(Tmp(i-1) + Tmp(i+1))
   end do

End Subroutine Relax

Subroutine Exchange_Halo(N, A)
!
! Exchange halo information between neighboring processes
!
   Implicit None
   Integer, intent(in   ) :: N
   Real,    intent(inout) :: A(-1:N+1)[*]
   Integer                :: left, right
   
   if (this_image() /= 1) then
      left  = this_image() - 1
   else
      left  = num_images()
   end if

   if (this_image() /= num_images()) then
      right = this_image() + 1
   else
      right = 1
   end if

   !! halo exchange between images
   !
   sync all   ! ensure that this image can read from neighbors

   A( -1) = A(N-1) [left ]
   A(N+1) = A(  1) [right]

   sync all   ! ensure that neighbors have read and thus this image can modify

End Subroutine Exchange_Halo

End Program PoissonMultigrid
