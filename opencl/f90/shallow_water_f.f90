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
   integer, parameter :: NPAD = 1
   integer, parameter :: NX  = 2*1280    ! factor of 2 because of float4
   integer, parameter :: NY  = 2*1280
end module shallow_water_mod

program shallow_water_f
   use, intrinsic :: ISO_C_BINDING
   use            :: shallow_water_mod
   use            :: Timer_mod
   implicit none

   interface
      subroutine wave_advance(H, U, V, dx, dt)
         implicit none
         real, target, dimension(:,:) :: H, U, V
         real, intent(in) :: dx, dt
      end subroutine wave_advance
   end interface

   real, target, dimension(NX+2*NPAD,NY+2*NPAD) :: H, U, V
   real :: dx, dt

   type(CPUTimer) :: timer
   real(c_double) :: time

   integer :: i, nLoops=100
   integer :: global_mem_size = (NX+2*NPAD)*(NY+2*NPAD)*4
   real :: throughput, flops

   ! initialize memory
   !

   dx = 1.0;  dt = 0.01;

   H = 1.0;  U = 0.0;  V = 0.0;

   print *
   print *, "Measuring flops and effective bandwidth of computation"

   call init(timer)
   call start(timer)
   do i = 1, nLoops
      call wave_advance(H, U, V, dx, dt)
   end do
   call stop(timer)
   time = elapsed_time(timer)

   print *, "   run time    ==   ", real(time)/nLoops
   ! 1.0e-9 -> GB, 1000 -> ms
   throughput = (1.0e-9 * 1000) * nLoops * global_mem_size / time
   print *, "   throughput    ==    ", throughput, "GB/s"

   ! 1.0e-9 -> GFlop, 1000 -> ms, 5 -> 4 sums / 1 div
   flops = (1.0e-9 * 1000) * nLoops * (100*NX*NY/time)
   print *, "   flops        ==    ", flops, "GFlops"

end program shallow_water_f


subroutine wave_advance(H, U, V, dx, dt)
   use :: regions
   use :: shallow_water_mod
   implicit none
   real, dimension(:,:)  :: H, U, V
   real, intent(in) :: dx, dt

   integer, dimension(4) :: halo, face_lt, face_rt, face_up, face_dn

   ! explicit temporaries
   real, allocatable, dimension(:,:) :: Hx, Hy, Ux, Uy, Vx, Vy

   ! pointers for interior and shifted regions
   real, pointer, dimension(:,:) :: iH, iU, iV

   ! scalar quantities
   real :: cs, gs, dtdx, dtdx2

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
   dtdx2 = 0.5 * dt/dx

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
        + dtdx2 * ( region(U, face_dn) - region(U, face_up) )

   ! x momentum
   Uy =     0.5 * ( region(U, face_dn) + region(U, face_up) )  &
        + dtdx2 * ( region(U, face_dn) * region(V, face_dn) / region(H, face_dn) )  &
        - dtdx2 * ( region(U, face_up) * region(V, face_up) / region(H, face_up) )

   ! x momentum
   Vy =     0.5 * ( region(V, face_dn) + region(V, face_up) )  &
        + dtdx2 * ( region(V, face_dn)**2 / region(H, face_dn) + gs*region(H, face_dn)**2 )  &
        - dtdx2 * ( region(V, face_up)**2 / region(H, face_up) + gs*region(H, face_up)**2 )

   ! second half step
   !

   face_lt = [0,1,0,0]
   face_rt = [1,0,0,0]
   face_dn = [0,0,0,1]
   face_up = [0,0,1,0]

   ! height
   iH = iH + dtdx * ( region(Ux, face_lt) - region(Ux, face_rt) ) &
           + dtdx * ( region(Vy, face_dn) - region(Vy, face_up) )

   ! x momentum
   iU = iU + dtdx * ( region(Ux, face_lt)**2 / region(Hx, face_lt) + gs*region(Hx, face_lt)**2 ) &
           - dtdx * ( region(Ux, face_rt)**2 / region(Hx, face_rt) + gs*region(Hx, face_rt)**2 ) &
           + dtdx * ( region(Uy, face_dn) * region(Vy, face_dn) / region(Hy, face_dn) )          &
           - dtdx * ( region(Uy, face_up) * region(Vy, face_up) / region(Hy, face_up) )

   ! y momentum
   iV = iV + dtdx * ( region(Ux, face_dn) * region(Vx, face_dn) / region(Hx, face_dn) )          &
           - dtdx * ( region(Ux, face_up) * region(Vx, face_up) / region(Hx, face_up) )          &
           + dtdx * ( region(Vy, face_lt)**2 / region(Hy, face_lt) + gs*region(Hy, face_lt)**2 ) &
           - dtdx * ( region(Vy, face_rt)**2 / region(Hy, face_rt) + gs*region(Hy, face_rt)**2 )

   ! clean up memory
   !
   deallocate( Hx, Ux, Vx, Hy, Uy, Vy)

end subroutine wave_advance
