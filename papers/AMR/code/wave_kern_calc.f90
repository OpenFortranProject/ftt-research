pure function calcDelta(dx)
   integer, parameter :: l=-1, r=+1
   real, intent(in) :: dx(l:r)
   real :: calcDeltaX(l:r)

   calcDeltaX(l) = dx(l) + dx(0)
   calcDeltaX(0) = dx(0)
   calcDeltaX(r) = dx(0) + dx(r)
end function calcDelta

pure function calcVarDist(Var,Dist)
   integer, parameter :: l=-1, r=+1
   real, intent(in) :: Var(l:r), Dist(l:r)
   real :: calcVarDist(l:r)

   calcVarDist(l) = dx(l) + dx(0)
   calcVarDist(0) = dx(0)
   calcVarDist(r) = dx(0) + dx(r)
end function calcDelta



pure concurrent &
subroutine calc_one_cycle(H, U, V, deltaT)
   real(C_DOUBLE), halo(1:*:1,1:*:1), halo_fn(amr_copy), &
                   intent(in out), dimension(0:,0:) :: H, U, V
   real(C_DOUBLE), intent(in) :: deltaT


!                 const int    ncells,   // 0  Total number of cells.
!                 const int    levmx,    // 1  Maximum level
!        __global const real  *H_old,    // 2
!        __global const real  *U_old,    // 3
!        __global const real  *V_old,    // 4
!        __global       real  *H_new,    // 5
!        __global       real  *U_new,    // 6
!        __global       real  *V_new,    // 7
!        __global const int   *nlft,     // 8  Array of left neighbors.
!        __global const int   *nrht,     // 9  Array of right neighbors.
!        __global const int   *ntop,     // 10  Array of bottom neighbors.
!        __global const int   *nbot,     // 11  Array of top neighbors.
!        __global const int   *level,    // 12  Array of level information.
!                 const real   deltaT,   // 14  Size of time step.
!        __global const real  *lev_dx,   // 15
!        __global const real  *lev_dy,   // 16
!        __local        real4 *tile,     // 17  Tile size in real4.
!        __local        int8  *itile)    // 18  Tile size in int8.

   !... local variables

   integer, parameter :: nl=-1, rc=+1, dc=-1, uc=+1   ! cell indices
   integer, parameter :: lf=0, rf=1, df=2, uf=3       ! face indices

   real(C_DOUBLE), dimension(l:r) :: H, U, V, dx, dy
   real(C_DOUBLE), dimension(l:r) :: H, Hx, Ux, Vx, deltaX
   real(C_DOUBLE), dimension(l:r) :: Hy, Uy, Vy, deltaY

   real(C_DOUBLE), dimension(l:r) :: H_face(4), U_face(4), V_face(4)


   !... face quantities
   real(C_DOUBLE), dimension(lf:uf) :: Hx


   H_face = H_face_fn(H, U, V, delta)
   U_face = U_face_fn(H, U, V, delta)
   V_face = V_face_fn(H, U, V, delta)

   H = H_cell_fn(H_face, U_face, V_face)
   U = U_cell_fn(H_face, U_face, V_face)
   V = V_cell_fn(H_face, U_face, V_face)


   ! 1D halo

   dx =
   dy =

   H = H_old??


   !///////////////////////////////////////////////////////////////////////
   !///                       Lax-Wendroff Method                       ///
   !///////////////////////////////////////////////////////////////////////

   !... x components

   dxminus =
   dxplus =
   dyminus =
   dyplus =
   



!!!!!!   deltaX = calcDelta(dx)

   Havg   = calcWeightedAvgX(H,dx)
   HxFlux = calcHxFlux(U)

   Hx(l) = ( Havg(l) + deltaT*(HxFlux(0) - HxFlux(l)) )/deltaX(l)
   Hx(r) = ( Havg(r) + deltaT*(HxFlux(r) - HxFlux(0)) )/deltaX(r)

   !... alternative
   Hx(l:r) = ( Havg(l:r) + deltaT*(HxFlux(0:r) - HxFlux(l:0)) )/deltaX(l:r)

   Uxdx   = calcUxdx(U,dx)
   UxFlux = calcUxFlux(U,H)

   Ux(l) = ( (Uxdx(l) + Uxdx(0)) + deltaT*(UxFlux(0) - UxFlux(l)) )/deltaX(l)
   Ux(r) = ( (Uxdx(0) + Uxdx(r)) + deltaT*(UxFlux(r) - UxFlux(0)) )/deltaX(r)

   !... alternative
   Ux(l:r) = ( (Uxdx(l:0) + Uxdx(0:r)) + deltaT*(UxFlux(0:r) - UxFlux(l:0)) )/deltaX(l:r)

   Vxdx   = calcVxdx(V,dx)
   UVFlux = calcUVFlux(U,V,H)

   Vx(l) = ( (Vxdx(l) + Vxdx(0)) + deltaT*(UVFlux(0) - UVFlux(l)) )/deltaX(l)
   Vx(r) = ( (Vxdx(0) + Vxdx(r)) + deltaT*(UVFlux(r) - UVFlux(0)) )/deltaX(r)

   !...alternative
   Vx(l:r) = ( (Vxdx(l:0) + Vxdx(0:r)) + deltaT*(UVFlux(0:r) - UVFlux(l:0)) )/deltaX(l:r)

   !... y components

   deltaY = calcDelta(dy)

   Hydy   = calcHydy(H,dy)
   HyFlux = calcHyFlux(V)

   Hx(b) = ( (Hydy(b) + Hydy(0)) + deltaT*(HyFlux(0) - HyFlux(b)) )/deltaY(b)
   Hx(t) = ( (Hydy(0) + Hydy(t)) + deltaT*(HyFlux(t) - HyFlux(0)) )/deltaY(t)

   !... alternative
   Hx(b:t) = ( (Hydy(b:0) + Hydy(0:t)) + deltaT*(HyFlux(0:t) - HyFlux(b:0)) )/deltaY(b:t)

   Uydy   = calcUydy(U,dy)
   VUFlux = calcVUFlux(U,V,H)

   Ux(b) = ( (Uydy(b) + Uydy(0)) + deltaT*(VUFlux(0) - VUFlux(b)) )/deltaY(b)
   Ux(t) = ( (Uydy(0) + Uydy(t)) + deltaT*(VUFlux(t) - VUFlux(0)) )/deltaY(t)

   Vydy   = calcVydy(V,dy)
   VyFlux = calcVyFlux(V,H)

   Vx(b) = ( (Vydy(b) + Vydy(0)) + deltaT*(VyFlux(0) - VyFlux(b)) )/deltaY(b)
   Vx(t) = ( (Vydy(0) + Vydy(t)) + deltaT*(VyFlux(t) - VyFlux(0)) )/deltaY(t)

   !///////////////////////////////////////////////////////////////////////
   !///////////////////////////////////////////////////////////////////////
   !///////////////////////////////////////////////////////////////////////
