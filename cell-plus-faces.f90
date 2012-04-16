module concurrent_mod
contains

   concurrent subroutine &
   map_to_face(H, U, V, Hx, Hy, Ux, Uy, Vx, Vy, delta, deltaT)
      implicit none
      real, intent(in),  halo(0:*:1,0:*:1) :: H(0:,0:), U(0:,0:), V(0:,0:)
      real, intent(out), dimension(0:,0:)  :: Hx, Hy, Ux, Uy, Vx, Vy

      real, dimension(2) :: dx, dy, HFlux, UFlux, VFlux
      real               :: dSum

      integer, parameter :: l = 0, r = 1
      integer, parameter :: b = 0, t = 1
      real   , parameter :: ghalf = 0.5*9.80   ! gravitational constant divided by 2

      ! x face values

      dx   = delta(l:r,0)
      dSum = dx(l) + dx(r)

      HFlux = U(l:r,0)
      UFlux = sqrt(U(l:r,0))/H(l:r,0) + ghalf*sqrt(H(l:r,0))
      VFlux = U(l:r,0)*V(l:r,0)/H(r:l,0)

      Hx(0,0) = ( (H(l,0)*dx(r) + H(r,0)*dx(l) + deltaT*(HFlux(r) - HFlux(l)) )/dSum
      Ux(0,0) = ( (U(l,0)*dx(r) + U(r,0)*dx(l) + deltaT*(UFlux(r) - UFlux(l)) )/dSum
      Vx(0,0) = ( (V(l,0)*dx(r) + V(r,0)*dx(l) + deltaT*(VFlux(r) - VFlux(l)) )/dSum

      ! y face values

      dy   = delta(0,b:t)
      dSum = dy(b) + dy(t)

      HFlux = V(0,b:t)
      UFlux = V(0,b:t)*U(0,b:t)/H(0,b:t)
      VFlux = sqrt(V(0,b:t))/H(0,b:t) + ghalf*sqrt(H(0,b:t))

      Hy(0,0) = ( (H(0,b)*dy(t) + H(0,t)*dy(b) + deltaT*(HFlux(t) - HFlux(b)) )/dSum
      Uy(0,0) = ( (U(0,b)*dy(t) + U(0,t)*dy(b) + deltaT*(UFlux(t) - UFlux(b)) )/dSum
      Vy(0,0) = ( (V(0,b)*dy(t) + V(0,t)*dy(b) + deltaT*(VFlux(t) - VFlux(b)) )/dSum

   end subroutine map_to_face

end module concurrent_mod

!
! mesh
!   l , left   cell center halo
!   r , right  cell center halo
!   b , bottom cell center halo
!   t , top    cell center halo
!   o , cell center
!   | , x face
!   - , extra halo element (not used)
!   : , extra face element (not used)
!
!  +   t   t   t   +
!  l | o | o | o | r
!  l | o | o | o | r
!  l | o | o | o | r
!  + : b : b : b : +
!
!  +   t   t   t   +
!  +   -   -   -   
!  l | o | o | o | r
!  +   -   -   -   
!  l | o | o | o | r
!  +   -   -   -   
!  l | o | o | o | r
!  +   -   -   -   
!  + + b + b + b + +
!  
!
program cell_plux_faces
   use :: concurrent_mod
   ! numCells = NX + 2 halo
   ! numFaces = NX + 1
   real, halo(1:*:1,1:*:1)    :: delta(NX,NY)
   real, halo(1:*:1,1:*:1)    :: H(NX,NY), U(NX,NY), V(NX,NY)
   real, dimension(0:NX,0:NY) :: Hx, Hy, Ux, Uy, Vx, Vy
   integer :: i, j

   ! interpolate (map) to faces (for all faces)
   !
   do concurrent(i=0:NX,j=0:NY)
      call map_to_face(H, U, V, Hx, Hy, Ux, Uy, Vx, Vy, delta, deltaT)
   end do

   !  (for all cells)
   !
   do concurrent(i=1:NX,j=1:NY)
      call collect_to_cell(H, Hx, Hy, delta)
   end do

end program
