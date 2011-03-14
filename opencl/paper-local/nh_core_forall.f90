module constants_mod
   real, parameter :: grav = 9.8      ! real value doesn't matter for now
end module constants_mod


module nh_core_forall_mod

! Notes:
! This module is used to test programming model concepts for
! programming accelerated processors.  The central idea is for
! the programming model to restrict the programmer so that
! the compiler is able to generate efficient massively threaded
! code.
!

   use constants_mod

   implicit none
   private

   public Riem_Solver_forall
   real, parameter:: dz_max = -0.5               ! (meters)

CONTAINS 

  subroutine Riem_Solver_forall(dt, is, ie, js, je, km, ng, &
                                akap, ptop, hs,             &
                                delp, gz)
!--------------------------------------------
! !OUTPUT PARAMETERS
! Output: gz: grav*height at edges
!--------------------------------------------
   implicit none
   integer, intent(in):: is, ie, js, je, km, ng
   real, intent(in):: dt         ! the BIG horizontal Lagrangian time step
   real, intent(in):: akap, ptop
   real, intent(in):: hs(is-ng:ie+ng,js-ng:je+ng)
   real, intent(inout), dimension(is-ng:ie+ng,js-ng:je+ng,km):: delp
   real, intent(out), dimension(is-ng:ie+ng,js-ng:je+ng,km+1):: gz

! Local:
   real, dimension(is:ie,km):: dm
   real    :: gama, rgrav, ptk
   integer :: i, j, k

!
! The first problem to solve is what is the dimension and size of
! the forall loop.  This is important because it relates to the
! number of threads running the problem.  It looks like this is
! fundamentally a 3D problem.
!

   forall (j=js:je, k=1:km, i=is:ie)

!
! Scalar assignments, not a problem as these scalars
! will have local storage and will be calculated for each thread.
! So logically they are within the loop which leads to compiler
! warnings.
!
       gama = 1./(1.-akap)
      rgrav = 1./grav
        ptk = ptop ** akap

!
! However, this is a problem as the arrays aren't the same size.
! This means we have to rethink the problem.  We could treat
! this as a 2D problem and call the kernel procedure for different
! j values.
!
      dm(i,k) = delp(i,j,k)

   end forall

end subroutine Riem_Solver_forall

end module nh_core_forall_mod
