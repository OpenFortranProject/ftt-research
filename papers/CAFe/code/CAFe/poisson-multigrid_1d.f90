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

Integer :: t, i, device
Integer :: nsteps = 5

Real, allocatable, dimension(:), codimension[*] :: V1h, V2h, V4h, V8h, Buf

device = get_subimage(0)

Allocate(V1h(-1:N+1)[*])
Allocate(Buf(-1:N+1)[*])
Allocate(V2h(-1:N/2+1)[*])
Allocate(V4h(-1:N/4+1)[*])
Allocate(V8h(-1:N/8+1)[*])

if (device /= THIS_IMAGE()) then
  Allocate(V1h(-1:N+1)[*])    [[device]]
  Allocate(Buf(-1:N+1)[*])    [[device]]
  Allocate(V2h(-1:N/2+1)[*])  [[device]]
  Allocate(V4h(-1:N/4+1)[*])  [[device]]
  Allocate(V8h(-1:N/8+1)[*])  [[device]]
end if

open(unit=fd, file="error_time.dat")

!! Initialize
!
V1h = 0.0
Call AddFourierMode(N, V1h,  1)
Call AddFourierMode(N, V1h,  6)
Call AddFourierMode(N, V1h, 16)
V1h = (1./3.)*V1h

V1h[device] = V1h

!... Relax solution on 1h mesh
!    -------------------------
call Textual_Output(N, V1h, "1h_0")
do t = 1, nsteps
  do concurrent(i=0:N)  [[device]]
    call Relax_1D(N, V1h, Buf)
  end do
  call Exchange_Halo_1D(device, N, V1h)
  write(fd, *) t, maxval(V1h)
end do
V1h = V1h[device]
call Textual_Output(N, V1h, "1h_mid")

!... Relax solution on 2h mesh
!    -------------------------
Call Restrict(N, V1h, V2h)
Call Textual_Output(N/2, V2h, "2h_0")
do t = 1, nsteps
  do concurrent(i=0:N)  [[device]]
    call Relax(N/2, V2h, Buf)
  end do
  Call Exchange_Halo_1D(device, N/2, V2h)
  write(fd, *) t, maxval(V2h)
end do
V2h = V2h[device]
Call Textual_Output(N/2, V2h, "2h_mid")

!... Relax solution on 4h mesh
!    -------------------------
Call Restrict(N/2, V2h, V4h)
Call Textual_Output(N/4, V4h, "4h_0")
do t = 1, nsteps
  do concurrent(i=0:N)  [[device]]
    call Relax(N/4, V4h, Buf)
  end do
  Call Exchange_Halo_1D(device, N/4, V4h)
  write(fd, *) t, maxval(V4h)
end do
V4h = V4h[device]
Call Textual_Output(N/4, V4h, "4h_mid")

!... Relax solution on 8h mesh
!    -------------------------
Call Restrict(N/4, V4h, V8h)
Call Textual_Output(N/8, V8h, "8h_0")
do t = 1, nsteps
  do concurrent(i=0:N)  [[device]]
    call Relax(N/8, V8h, Buf)
  end do
  Call Exchange_Halo_1D(device, N/8, V8h)
  write(fd, *) t, maxval(V8h)
end do
V8h = V8h[device]
Call Textual_Output(N/8, V8h, "8h_mid")

!! IMPORTANT: this last step should be an exact solution on a smaller grid probably
!
! exact solution is 0, so just set to 0.

Buf = 0

Buf[device] = Buf
Call Prolongate(N/8, V8h, Buf)
do t = 1, nsteps
  do concurrent(i=0:N)  [[device]]
    call Relax(N/8, V8h, Buf)
  end do
  Call Exchange_Halo_1D(device, N/8, V8h)
  write(fd, *) t, maxval(V8h)
end do
V8h = V8h[device]
Call Textual_Output(N/8, V8h, "8h_end")

Call Prolongate(N/4, V4h, V8h)
do t = 1, nsteps
  do concurrent(i=0:N)  [[device]]
    call Relax(N/4, V4h, Buf)
  end do
  Call Exchange_Halo_1D(device, N/4, V4h)
  write(fd, *) t, maxval(V4h)
end do
V4h = V4h[device]
Call Textual_Output(N/4, V4h, "4h_end")

Call Prolongate(N/2, V2h, V4h)
do t = 1, nsteps
  do concurrent(i=0:N)  [[device]]
    call Relax(N/2, V2h, Buf)
  end do
  Call Exchange_Halo_1D(device, N/2, V2h)
  write(fd, *) t, maxval(V2h)
end do
V2h = V2h[device]
Call Textual_Output(N/2, V2h, "2h_end")

Call Prolongate(N, V1h, V2h)
do t = 1, nsteps
  do concurrent(i=0:N)  [[device]]
    call Relax_1D(N, V1h, Buf)
  end do
  call Exchange_Halo_1D(device, N, V1h)
  write(fd, *) t, maxval(V1h)
end do
V1h = V1h[device]
Call Textual_Output(N, V1h, "1h_end")

close(fd)

CONTAINS

Pure Subroutine Relax_1D(N, A, Tmp)
!
! Relax on the interior and the two halo cells shared with the left and right neighbors
!   - shared halo cells are computed twice and are not exchanged
!   - the outside halo cells are from neighbors and cannot be not computed
!
   Implicit None
   Integer, intent(in   ) :: N
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
   Tmp(0) = 0.0
   Tmp(N) = 0.0

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
   A(0) = 0.0
   A(N) = 0.0

End Subroutine Relax

Subroutine Exchange_Halo_1D(N, A)
!
! Exchange halo information between neighboring processes
!
   Implicit None
   Integer, intent(in   ) :: N
   Real,    intent(inout) :: A(-1:N+1)[*]

   integer :: left, right

   left  = THIS_IMAGE() - 1
   right = THIS_IMAGE() + 1

   if (left  < 1)             left  = NUM_IMAGES()
   if (right > NUM_IMAGES())  right = 1

   !! Exchange with neighbors (copy)
   !
   A( -1) = A(N-1) [left ]
   A(N+1) = A(  1) [right]

   !! Compute common boundaries now that we have halo computed at complete relax step
   !
   A(0) = (1.0 - w)*A(0) + 0.5*w*(A( -1) + A( +1))
   A(N) = (1.0 - w)*A(N) + 0.5*w*(A(N-1) + A(N+1))

   !! Reset boundary conditions (in case they've changed, why should they?)
   !
   if (THIS_IMAGE() == 1           ) A(0) = 0.0
   if (THIS_IMAGE() == NUM_IMAGES()) A(N) = 0.0

End Subroutine Exchange_Halo_1D

Subroutine Get_Halo(device, N, A)
   TYPE(CLDevice), intent(in) :: device
   Integer, intent(in)        :: N
   Real, intent(inout)        :: A(-1:N+1)[*]

   !! Copy halo regions from the device
   !
   A(-1:0  ) = A(-1:0  ) [device]
   A( N:N+1) = A( N:N+1) [device]

End Subroutine Get_Halo

Subroutine Put_Halo(device, N, A)
   TYPE(CLDevice), intent(in) :: device
   Integer, intent(in)        :: N
   Real, intent(inout)        :: A(-1:N+1)[*]

   !! Copy halo regions back to the device
   !
   A(-1:0  ) [device] = A(-1:0  )
   A( N:N+1) [device] = A( N:N+1)

End Subroutine Put_Halo

End Program PoissonMultigrid
