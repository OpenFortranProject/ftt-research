module constants_mod
   real, parameter :: grav = 9.8      ! real value doesn't matter for now
end module constants_mod


module nh_core_forall_concurrent

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

  subroutine Riem_3D_concur(ns, bdt, is, ie, js, je, ng, km, cp, gama, cappa, p3, dm3,    &
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

end subroutine Riem_3D_concur


subroutine Riem_3D_concur(ns, bdt, is, ie, js, je, ng, j, km, cp, gama, cappa, p3, dm2,    &
                          pm2, w, dz2, pt, quick_p, c_core, ktop, iad)

  integer, intent(in):: ns, is, ie, js, je, ng,  km, j
  integer, intent(in):: iad      ! time step scheme 
  integer, intent(in):: ktop     ! starting layer for non-hydrostatic dynamics
                                 ! 1: All non-hydrostatic
                                 ! 2: top sponge layer is hydrostatic
  real,    intent(in):: bdt, cp, gama, cappa
  real,    intent(in), dimension(is:ie,km):: dm2, pm2
  logical, intent(in):: quick_p       ! fast algorithm for pressure
  logical, intent(in):: c_core
  real, intent(in  ) :: pt (is-ng:ie+ng,js-ng:je+ng,km)
! IN/OUT:
  real, intent(inout):: dz2(is:ie,km)
  real, intent(inout)::   w(is-ng:ie+ng,js-ng:je+ng,km)
  real, intent(out  )::  p3(is-ng:ie+ng,js-ng:je+ng,km+1)
! --- Local 1D copyies -----
#ifdef USE_2D
  real, dimension(km,is:ie):: t2, p2, pt2
#else
  real, dimension(km):: c2, p2, pt2
#endif
  real, dimension(km):: r_p, r_n, rden, dz, dm, wm, dts, pdt
  real, dimension(km+1):: m_bot, m_top, r_bot, r_top, time_left, pe1, pbar, wbar

  real, parameter:: dzmx = 0.5*dz_max
  real    :: dt, rdt, grg, z_frac, t_left
  real    :: a1, b1, g2, rcp
  real    :: seq(ns)       ! time stepping sequence
  integer :: k2(km+1)
  integer :: i, k, n, ke, kt, k0, k1, k3

  call time_sequence( iad, ns, bdt, seq )

  grg = gama * rdgas  
  rcp = 1. / cp
  rdt = 1. / bdt

  if ( quick_p ) then
       a1 = 0.5               ! a1=1 fully implicit
       b1 = 1. - a1
       g2 = -2.*gama
  endif

#ifdef USE_2D
  do i=is,ie
     do k=ktop,km
        rden(k) = -rdgas*dm2(i,k)/dz2(i,k)
        pt2(k,i) = pt(i,j,k) * rcp
         p2(k,i) = ( rden(k)*pt2(k,i) )**gama
         t2(k,i) = p2(k,i) / rden(k)
     enddo
  enddo
#endif

  do k=1,km+1
     wbar(k) = 0.
     pbar(k) = 0.
  enddo

 do 6000 i=is,ie

    do k=ktop,km
#ifdef USE_2D
       dz(k) = dz2(i,k)
       dm(k) = dm2(i,k)
       wm(k) = w(i,j,k)*dm(k)
#else
       dz(k) = dz2(i,k)
       dm(k) = dm2(i,k)
       wm(k) = w(i,j,k)*dm(k)
       rden(k) = -rdgas*dm(k)/dz(k)
       pt2(k) = pt(i,j,k) * rcp   ! virtual effect is included in pt
!      p2(k) = ( rden(k)*pt2(k) )**gama
       p2(k) = exp( gama*log(rden(k)*pt2(k)) )
       c2(k) = sqrt( grg*p2(k)/rden(k) )
#endif
    enddo

    do k=1,km+1
       pe1(k) = 0.
    enddo

 do 5000 n=1,ns

   dt = seq(n)

   do k=ktop,km
#ifdef USE_2D
       dts(k) = -dz(k)/(sqrt(grg*t2(k,i)))
       pdt(k) = dts(k)*(p2(k,i)-pm2(i,k))
#else
       dts(k) = -dz(k) / c2(k)
       pdt(k) = dts(k)*(p2(k)-pm2(i,k))
#endif
       r_p(k) = wm(k) + pdt(k)
       r_n(k) = wm(k) - pdt(k)
   enddo

!--------------------------------------------------
! Compute r_top from bottom up: dm/dt > 0
!----------------------------------------------------
   do k=ktop+1,km+1
      k2(k) = k-1
      m_top(k) = 0.
      r_top(k) = 0.
      time_left(k) = dt
   enddo
  
   do 444 ke=km+1,ktop+1,-1
        kt=k2(ke)
     do k=kt,ktop,-1
        z_frac = time_left(ke)/dts(k)
        if ( z_frac <= 1. ) then
            if ( (ke-k) > 2 ) then
               k1 = ke-1
               k2(k1) = k
               m_top(k1) = m_top(ke) - dm(k1)
               r_top(k1) = r_top(ke) - r_n(k1)
               time_left(k1) = time_left(ke) + dts(k1)
            endif
            m_top(ke) = m_top(ke) + z_frac*dm(k)
            r_top(ke) = r_top(ke) + z_frac*r_n(k)
            go to 444
        else
            time_left(ke) = time_left(ke) - dts(k)
            m_top(ke) = m_top(ke) + dm(k)
            r_top(ke) = r_top(ke) + r_n(k)
        endif 
     enddo
! wave from ke already left the top
     if ( ke == ktop+1 ) exit
     do k=ke-1,ktop+1,-1
        m_top(k) = m_top(k+1) - dm(k)
        r_top(k) = r_top(k+1) - r_n(k)
     enddo
     exit
444 continue

!--------------------------------------------------
! Compute r_bot from top down: dm/dt < 0
!----------------------------------------------------
   do k=ktop,km
        k2(k) = k
     m_bot(k) = 0.
     r_bot(k) = 0.
    time_left(k) = dt
   enddo

  do 4000 ke=ktop,km
        kt = k2(ke)
     do k=kt,km
        z_frac = time_left(ke)/dts(k)
        if ( z_frac <= 1. ) then
             if ( (k-ke)>1 ) then
                time_left(ke+1) = time_left(ke) + dts(ke)
                m_bot(ke+1) =  m_bot(ke) - dm(ke)
                r_bot(ke+1) =  r_bot(ke) - r_p(ke)
                k2(ke+1) = k
             endif
                m_bot(ke) = m_bot(ke) + z_frac*dm(k)
                r_bot(ke) = r_bot(ke) + z_frac*r_p(k)
             if( ke==km ) go to 7777      ! All done
             go to 4000      ! to next interface
        else 
             time_left(ke) = time_left(ke) - dts(k)
             m_bot(ke) =  m_bot(ke) + dm(k)
             r_bot(ke) =  r_bot(ke) + r_p(k)
        endif
     enddo
!----------------------------------------
! Ray from edge-ke already hit the ground.
!----------------------------------------
     k3 = ke
     t_left = time_left(ke)
     exit
4000 continue


!---------------------------------
! Perfect reflection at the bottom
!---------------------------------
   k1 = km
   do kt=k3,km
     k0 = k1
     do k=k0,ktop,-1
        z_frac = t_left/dts(k)
        if ( z_frac <= 1. ) then
!-- next interface -------------------------------------
!          if ( kt /= km ) then
                    k1 = k
                 t_left = t_left + dts(kt)
                 m_bot(kt+1) = m_bot(kt) - dm(kt)
                 r_bot(kt+1) = r_bot(kt) - r_p(kt)
!          endif
!-------------------------------------------------------
           m_bot(kt) = m_bot(kt) + z_frac*dm(k)
           r_bot(kt) = r_bot(kt) - z_frac*r_n(k)
           exit            ! goto next interface
        else 
           m_bot(kt) = m_bot(kt) + dm(k)
           r_bot(kt) = r_bot(kt) - r_n(k)
              t_left = t_left - dts(k)
        endif
     enddo
   enddo

7777  continue

  pbar(ktop) = 0.
  wbar(ktop) = r_bot(ktop) / m_bot(ktop)
  do k=ktop+1,km
     wbar(k) = (r_bot(k)+r_top(k)) / (m_top(k)+m_bot(k))
     pbar(k) =  m_top(k)*wbar(k) - r_top(k)
  enddo
  pbar(km+1) = -r_top(km+1)
! wbar(km+1) = 0.

   do k=ktop+1,km+1
      pe1(k) = pe1(k) + pbar(k)
   enddo

   if ( n==ns ) then
      if ( c_core ) then
          do k=ktop,km
             dz2(i,k) = min(dzmx, dz(k) + dt*(wbar(k+1)-wbar(k)) )
          enddo
      else
          do k=ktop,km
             dz2(i,k) = min(dzmx, dz(k) + dt*(wbar(k+1)-wbar(k)) )
             w(i,j,k) = ( wm(k) + pbar(k+1) - pbar(k) ) / dm(k)
          enddo
      endif
   else
     if ( quick_p ) then
        do k=ktop,km
           wm(k) = wm(k) + pbar(k+1) - pbar(k)
           rden(k) = dt*(wbar(k+1)-wbar(k))   ! dz tendency
           pdt(k) = dz(k)                     ! old dz
           dz(k) = dz(k) + rden(k)            ! updated dz
           pdt(k) = g2*rden(k) / (pdt(k)+dz(k))
#ifdef USE_2D
           p2(k,i) = max(0., p2(k,i)*(1.+b1*pdt(k))/(1.-a1*pdt(k)))
           t2(k,i) = -p2(k,i)*dz(k)/(rdgas*dm(k))
#else
           p2(k) = max(0., p2(k)*(1.+b1*pdt(k))/(1.-a1*pdt(k)))
           c2(k) = sqrt( -gama*p2(k)*dz(k)/dm(k) )
#endif
        enddo
      else
        do k=ktop,km
           wm(k) = wm(k) + pbar(k+1) - pbar(k)
           dz(k) = min(dzmx, dz(k) + dt*(wbar(k+1)-wbar(k)) )
           rden(k) = -rdgas*dm(k)/dz(k)
#ifdef USE_2D
           p2(k,i) = (rden(k)*pt2(k,i))**gama
           t2(k,i) = p2(k,i)/rden(k)
#else
!          p2(k) = (rden(k)*pt2(k))**gama
           p2(k) = exp( gama*log(rden(k)*pt2(k)) )
           c2(k) = sqrt( grg*p2(k)/rden(k) )
#endif
        enddo
      endif
   endif

5000  continue

!---------------------------------------------------------------------
! Note: time-mean full pressure at edges and time-level-(n+1) DZ are
! used for computation of p-gradient
! Could use cell center full pressure at time-level-(n+1)
!---------------------------------------------------------------------
!        p3(i,j,1:ktop) = 0.
      do k=1,ktop
         p3(i,j,k) = 0.
      enddo
      do k=ktop+1,km+1
         p3(i,j,k) = pe1(k)*rdt
      enddo

6000  continue

 end subroutine Riem_3D

 subroutine time_sequence ( iad, ns, bdt, tseq )
 integer, intent(in) :: iad, ns
 real,    intent(in) :: bdt
 real, intent(out):: tseq(ns)
! local
  integer :: seq(ns)
  integer :: n, nstep
  real :: sdt

! Default uniform time stepping (iad=0)
  do n=1,ns
     seq(n) = 1
  enddo

! Note: iad=2 or 4 appear to be more stable than other options
  if ( ns>3 ) then
    if ( iad==1 ) then
                     ! 1, 1, 2, 2, ...
         do n=3,ns
            seq(n) = 2
         enddo
    elseif ( iad==2 ) then
                     ! 1, 2, 2, 2, ...
         do n=2,ns
            seq(n) = 2
         enddo
    elseif ( iad==3 ) then
                     ! 1, 2, 3, 3, 3. ...
         seq(2) = 2
         do n=3,ns
            seq(n) = 3
         enddo
    elseif ( iad==4 ) then
                     ! 1, 2, 4, 4, 4, ...
         seq(2) = 2
         do n=3,ns
            seq(n) = 4
         enddo
    elseif( iad==5 ) then
!---------------------
! Fibonacci sequence:
!---------------------
                     ! 1, 1, 2, 3, 5, 8, 13, 21, 34
         do n=3,ns
            seq(n) = seq(n-2) + seq(n-1)
         enddo
    endif
  endif

  nstep = 1
  if ( ns>1 ) then
       do n=2,ns
          nstep = nstep + seq(n) 
       enddo
  endif
  sdt = bdt / real(nstep)

  do n=1,ns
     tseq(n) = sdt * real ( seq(n) )
  enddo


 end subroutine time_sequence

end module nh_core_mod
