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

   public Riem_3D_forall
   real, parameter:: dz_max = -0.5               ! (meters)

CONTAINS 

  subroutine Riem_3D_forall(ns, bdt, is, ie, js, je, ng, km, cp, gama, cappa, p3, dm3,    &
                            pm3, w, dz2, pt, quick_p, c_core, ktop, iad)
!--------------------------------------------
! !OUTPUT PARAMETERS
! Output: gz: grav*height at edges
!--------------------------------------------
   implicit none
  integer, intent(in):: ns, is, ie, js, je, ng,  km
  integer, intent(in):: iad      ! time step scheme 
  integer, intent(in):: ktop     ! starting layer for non-hydrostatic dynamics
                                 ! 1: All non-hydrostatic
                                 ! 2: top sponge layer is hydrostatic
  real,    intent(in):: bdt, cp, gama, cappa
  real,    intent(in), dimension(is:ie,js:je,km):: dm3, pm3
  logical, intent(in):: quick_p       ! fast algorithm for pressure
  logical, intent(in):: c_core
  real, intent(in  ) :: pt (is-ng:ie+ng,js-ng:je+ng,km)
! IN/OUT:
  real, intent(inout):: dz2(is:ie,km)
  real, intent(inout)::   w(is-ng:ie+ng,js-ng:je+ng,km)
  real, intent(out  )::  p3(is-ng:ie+ng,js-ng:je+ng,km+1)

! --- Local 1D copies -----
  real, dimension(km):: c2, p2, pt2
  real, dimension(km):: r_p, r_n, rden, dz, dm, wm, dts, pdt
  real, dimension(km+1):: m_bot, m_top, r_bot, r_top, time_left, pe1, pbar, wbar

! Local scalars:
   integer :: i, j, k

!
! The first problem to solve is what is the dimension and size of
! the forall loop.  This is important because it relates to the
! number of threads running the problem.  It looks like this is
! fundamentally a 3D problem.  The way the loops are structured
! it looks like we have to host the j loop out of the procedure
! kernel and let the forall loop over i and k.  It would be
! better to do the forall over i and j (contiguous memory) but
! that is not the way the code is structured.

   do concurrent (i=is:ie, j=js:je)
      block
         real, dimension(km) :: dm, wm

         do k=ktop,km
            dm(k) = dm3(i,j,k)
            wm(k) = w(i,j,k)*dm(k)
         enddo

      end block
   end do concurrent

end subroutine Riem_3D_forall

end module nh_core_forall_mod
