module regions
contains

   function region(A, halo) result(pA)
      real, target,  dimension(:,:) :: A
      real, pointer, dimension(:,:) :: pA
      integer, intent(in), dimension(4) :: halo
      integer, dimension(2) :: lb, ub

      lb = lbound(A)
      ub = ubound(A)
      pA => A(lb(1)+halo(1):ub(1)-halo(2), lb(2)+halo(3):ub(2)-halo(4))

   end function region

   function transfer_halo(A, halo) result(pA)
      real, target,  dimension(:,:) :: A
      real, pointer, dimension(:,:) :: pA
      integer, intent(in), dimension(4) :: halo
      integer, dimension(2) :: lb, ub

      lb = lbound(A)
      ub = ubound(A)
      pA => A(lb(1)+halo(1):ub(1)-halo(2), lb(2)+halo(3):ub(2)-halo(4))

   end function transfer_halo


end module regions

module shallow_water_mod
   integer, parameter :: G_WIDTH = 1280
   integer, parameter :: NPAD = 1
   integer, parameter :: NX = G_WIDTH
   integer, parameter :: NY = G_WIDTH
end module shallow_water_mod

program shallow_water_f
   use, intrinsic :: ISO_C_BINDING
   use            :: shallow_water_mod
   use            :: Timer_mod
   implicit none

   interface
      subroutine wave_advance(H, U, V, dx, dy, dt)
         implicit none
         real, target, dimension(:,:) :: H, U, V
         real, intent(in) :: dx, dy, dt
      end subroutine wave_advance
      subroutine wave_advance_loops(H, U, V, dx, dy, dt)
         use :: shallow_water_mod
         implicit none
         real, target, dimension(NX+2*NPAD,NY+2*NPAD) :: H, U, V
         real, intent(in) :: dx, dy, dt
      end subroutine wave_advance_loops
   end interface

   real, target, dimension(NX+2*NPAD,NY+2*NPAD) :: H, U, V
   real :: dx, dy, dt

   type(CPUTimer) :: timer
   real(c_double) :: h_time

   integer :: i, nLoops=100, nWarm=20
   integer :: global_mem_size = (NX+2*NPAD)*(NY+2*NPAD)*4
   real :: throughput, flops

   ! initialize memory
   !

   dx = 1.0;  dy = 1.0;  dt = 0.025;

   H = 1.0;  U = 0.0;  V = 0.0;

   ! warmup the kernel
   !
   print *, "warmup"
   do i = 1, nWarm
      call wave_advance_loops(H, U, V, dx, dy, dt)
   end do

   print *
   print *, "Measuring flops and effective bandwidth of computation"

   call init(timer)
   call start(timer)
   do i = 1, nLoops
      call wave_advance_loops(H, U, V, dx, dy, dt)
   end do
   call stop(timer)
   h_time = elapsed_time(timer)

   h_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(h_time)/nLoops, " msec (avg)"

   throughput = (1.0e-9 * 1000) * nLoops * global_mem_size / h_time
   print *, "   throughput    ==    ", throughput, "GB/s"

   ! 1.0e-9 -> GFlop, 1000 -> ms, 5 -> 4 sums / 1 div
   flops = (1.0e-9 * 1000) * nLoops * (100*NX*NY/h_time)
   print *, "   flops        ==    ", flops, "GFlops"

end program shallow_water_f

subroutine wave_advance(H, U, V, dx, dy, dt)
   use :: regions
   use :: shallow_water_mod
   implicit none
   real, dimension(:,:)  :: H, U, V
   real, intent(in) :: dx, dy, dt

   integer, dimension(4) :: halo, face_lt, face_rt, face_up, face_dn

   ! pointers for interior and shifted regions
   real, pointer, dimension(:,:) :: iH, iU, iV

   ! explicit temporaries
   real, allocatable, dimension(:,:) :: Hx, Hy, Ux, Uy, Vx, Vy

   ! scalar quantities
   real :: cs, gs, dtdx, dtdy, dtdx2, dtdy2

   ! transfer data first
   !
   halo = [NPAD, NPAD, NPAD, NPAD]

   iH => transfer_halo(H, halo)
   iU => transfer_halo(U, halo)
   iV => transfer_halo(V, halo)

   ! allocate memory for temporary arrays
   !
   allocate( Hx(NX+NPAD,NY) )
   allocate( Ux(NX+NPAD,NY) )
   allocate( Vx(NX+NPAD,NY) )

   allocate( Hy(NX,NY+NPAD) )
   allocate( Uy(NX,NY+NPAD) )
   allocate( Vy(NX,NY+NPAD) )

   ! first half step
   !

   gs = 0.5 * 9.8
   dtdx = dt/dx
   dtdy = dt/dy
   dtdx2 = 0.5 * dt/dx
   dtdy2 = 0.5 * dt/dy

   face_lt = [0,1,1,1]
   face_rt = [1,0,1,1]
   face_dn = [1,1,0,1]
   face_up = [1,1,1,0]

   ! height
   Hx =     0.5 * ( region(H, face_lt) + region(H, face_rt) )  &
        + dtdx2 * ( region(U, face_lt) - region(U, face_rt) )

   ! x momentum
   Ux =     0.5 * ( region(U, face_lt) + region(U, face_rt) )  &
        + dtdx2 * ( region(U, face_lt)**2 / region(H, face_lt) + gs*region(H, face_lt)**2 )  &
        - dtdx2 * ( region(U, face_rt)**2 / region(H, face_rt) + gs*region(H, face_rt)**2 )

   ! x momentum
   Vx =     0.5 * ( region(V, face_lt) + region(V, face_rt) )  &
        + dtdx2 * ( region(U, face_lt) * region(V, face_lt) / region(H, face_lt) )  &
        - dtdx2 * ( region(U, face_rt) * region(V, face_rt) / region(H, face_rt) )

   ! height
   Hy =     0.5 * ( region(H, face_dn) + region(H, face_up) )  &
        + dtdy2 * ( region(U, face_dn) - region(U, face_up) )

   ! x momentum
   Uy =     0.5 * ( region(U, face_dn) + region(U, face_up) )  &
        + dtdy2 * ( region(U, face_dn) * region(V, face_dn) / region(H, face_dn) )  &
        - dtdy2 * ( region(U, face_up) * region(V, face_up) / region(H, face_up) )

   ! x momentum
   Vy =     0.5 * ( region(V, face_dn) + region(V, face_up) )  &
        + dtdy2 * ( region(V, face_dn)**2 / region(H, face_dn) + gs*region(H, face_dn)**2 )  &
        - dtdy2 * ( region(V, face_up)**2 / region(H, face_up) + gs*region(H, face_up)**2 )

   ! second half step
   !

   face_lt = [0,1,0,0]
   face_rt = [1,0,0,0]
   face_dn = [0,0,0,1]
   face_up = [0,0,1,0]

   ! height
   iH = iH + dtdx * ( region(Ux, face_lt) - region(Ux, face_rt) ) &
           + dtdy * ( region(Vy, face_dn) - region(Vy, face_up) )

   ! x momentum
   iU = iU + dtdx * ( region(Ux, face_lt)**2 / region(Hx, face_lt) + gs*region(Hx, face_lt)**2 ) &
           - dtdx * ( region(Ux, face_rt)**2 / region(Hx, face_rt) + gs*region(Hx, face_rt)**2 ) &
           + dtdy * ( region(Uy, face_dn) * region(Vy, face_dn) / region(Hy, face_dn) )          &
           - dtdy * ( region(Uy, face_up) * region(Vy, face_up) / region(Hy, face_up) )

   ! y momentum
   iV = iV + dtdx * ( region(Ux, face_dn) * region(Vx, face_dn) / region(Hx, face_dn) )          &
           - dtdx * ( region(Ux, face_up) * region(Vx, face_up) / region(Hx, face_up) )          &
           + dtdy * ( region(Vy, face_lt)**2 / region(Hy, face_lt) + gs*region(Hy, face_lt)**2 ) &
           - dtdy * ( region(Vy, face_rt)**2 / region(Hy, face_rt) + gs*region(Hy, face_rt)**2 )

   ! clean up memory
   !
   deallocate( Hx, Ux, Vx, Hy, Uy, Vy)

end subroutine wave_advance


!
! make this as fast as possible using normal Fortran programming style
! with loop constructs
!
subroutine wave_advance_loops(H, U, V, dx, dy, dt)
   use :: regions
   use :: shallow_water_mod
   implicit none
   real, dimension(NX+2*NPAD,NY+2*NPAD) :: H, U, V
   real, intent(in) :: dx, dy, dt

   integer :: i, j, ils, ile, jls, jle

   integer, dimension(4) :: halo, face_lt, face_rt, face_up, face_dn

   ! pointers for interior and shifted regions
   real :: iH, iU, iV

   ! explicit automatic temporaries
   real, dimension(NX+NPAD,NY) :: Hx, Ux, Vx
   real, dimension(NX,NY+NPAD) :: Hy, Uy, Vy

   ! scalar quantities
   real :: cs, gs, dtdx, dtdy, dtdx2, dtdy2

   ! first half step
   !

   gs = 0.5 * 9.8
   dtdx = dt/dx
   dtdy = dt/dy
   dtdx2 = 0.5 * dt/dx
   dtdy2 = 0.5 * dt/dy

   face_lt = [0,1,1,1]
   face_rt = [1,0,1,1]
   face_dn = [1,1,0,1]
   face_up = [1,1,1,0]

   ! interior indices
   !   i = 1:NX+2  j = 1:NY+2
   !
   ils = 2;  ile = NX+2*NPAD - 1
   jls = 2;  jle = NY+2*NPAD - 1

   ! x faces
   !   face_lt = (i, j+1)
   !   face_rt = (i+1, j+1)
   !
   do j = 1, NY                   ! interior j = 2:NY+1
      do i = 1, NX+NPAD           ! interior i = 1:NX+1, 2:NX+2
         ! height
         Hx(i,j) =     0.5 * ( H(i,j+1) + H(i+1,j+1) )  &
                   + dtdx2 * ( U(i,j+1) - U(i+1,j+1) )
         ! x momentum
         Ux(i,j) =     0.5 * ( U(i,j+1) + U(i+1,j+1) )  &
                   + dtdx2 * ( U(i,j+1)**2   / H(i,j+1)   + gs*H(i,j+1)**2 )  &
                   - dtdx2 * ( U(i+1,j+1)**2 / H(i+1,j+1) + gs*H(i+1,j+1)**2 )
         ! x momentum
         Vx(i,j) =     0.5 * ( V(i,j+1) + V(i+1,j+1) )  &
                   + dtdx2 * ( U(i,j+1)   * V(i,j+1)   / H(i,j+1) )  &
                   - dtdx2 * ( U(i+1,j+1) * V(i+1,j+1) / H(i+1,j+1) )
      end do
   end do

   ! y faces
   !   face_dn = (i+1, j)
   !   face_up = (i+1, j+1)
   !
   do j = 1, NY+NPAD              ! interior j = 1:NY+1, 2:NY+2
      do i = 1, NX                ! interior i = 2:NX+1
         ! height
         Hy(i,j) =     0.5 * ( H(i+1,j) + H(i+1,j+1) )  &
                   + dtdy2 * ( U(i+1,j) - U(i+1,j+1) )

         ! x momentum
         Uy(i,j) =     0.5 * ( U(i+1,j) + U(i+1,j+1) )  &
                   + dtdy2 * ( U(i+1,j)   * V(i+1,j)   / H(i+1,j) )  &
                   - dtdy2 * ( U(i+1,j+1) * V(i+1,j+1) / H(i+1,j+1) )

         ! x momentum
         Vy(i,j) =     0.5 * ( V(i+1,j) + V(i+1,j+1) )  &
                   + dtdy2 * ( V(i+1,j)**2   / H(i+1,j)   + gs*H(i+1,j)**2 )  &
                   - dtdy2 * ( V(i+1,j+1)**2 / H(i+1,j+1) + gs*H(i+1,j+1)**2 )
      end do
   end do

   ! second half step
   !
   !  face_lt = (i-1, j-1)
   !  face_rt = (i, j-1)
   !  face_dn = (i-1, j-1)
   !  face_up = (i-1, j)
   !
   do j = 2, NY+1
      do i = 2, NX+1
         ! height
         H(i,j) = H(i,j) + dtdx * ( Ux(i-1,j-1) - Ux(i,j-1) ) &
                         + dtdy * ( Vy(i-1,j-1) - Vy(i-1,j) )
         ! x momentum
         U(i,j) = U(i,j) + dtdx * ( Ux(i-1,j-1)**2 / Hx(i-1,j-1) + gs*Hx(i-1,j-1)**2 ) &
                         - dtdx * ( Ux(i,j-1)**2   / Hx(i,j-1)   + gs*Hx(i,j-1)**2 ) &
                         + dtdy * ( Uy(i-1,j-1) * Vy(i-1,j-1) / Hy(i-1,j-1) )          &
                         - dtdy * ( Uy(i-1,j)   * Vy(i-1,j)   / Hy(i-1,j) )
         ! y momentum
         V(i,j) = V(i,j) + dtdx * ( Ux(i-1,j-1) * Vx(i-1,j-1) / Hx(i-1,j-1) )          &
                         - dtdx * ( Ux(i-1,j)   * Vx(i-1,j) / Hx(i-1,j) )          &
                         + dtdy * ( Vy(i-1,j-1)**2 / Hy(i-1,j-1) + gs*Hy(i-1,j-1)**2 ) &
                         - dtdy * ( Vy(i,j-1)**2   / Hy(i,j-1)   + gs*Hy(i,j-1)**2 )
      end do
   end do

end subroutine wave_advance_loops
