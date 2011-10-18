! +   Basic advection routine

Pure Elemental &
Subroutine Advect_Basic (id, Array, Eps, dFlux, lVol, lUpw, lDnw)

!==============================================================================
!  Description:  The basic advection routine for all variables, except the
!                three hydrodynamic variables.
!
!  Method:  The donor, upwind, and downwind advection quantities are computed.
!           The advection is performed and put back into "Array"
!
!  Reference: PAGOSA physics Manual,  LA-14425-M,  August 2010,  Los Alamos 
!             National Laoratory,  p. 42-48 and p. 183-193, p. 197-202.
!
!  Externals:
!      global_eoshift
! 
!  History:              Date            Author
!  Version
!  ----------         ----------      -------------
!   17.0              07/07/2009      Wayne Weseloh
!
!=============================================================================

!$OFP concurrent :: Advect_Basic

Use Kind_Module,   only: kint, kreal
Use Param_Module,  only: mx, my, mz

Implicit None
Save

!... Scalars:
Integer(kind=kint),  intent(in) :: id    !  advection sweep direction id

!... Arrays:
!----------------------------------------------------------------------------
!Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz), intent(inout) :: Array ! advection array section
!Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: Eps   ! residual donor mass/volume fraction
!Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: dFlux ! cell face mass/volume flux
!Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: TmpA  !
!Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: TmpB  !
!----------------------------------------------------------------------------
Real(kind=kreal), intent(inout) :: Array
Real(kind=kreal), intent(in)    :: Eps, lVol, lUpw, lDnw, dFlux

!$OFP elemental :: Array, Eps, lVol, lUpw, lDnw


! ... Shift to get Donor, Upwind, and Downwind values
!----------------------------------------------------------------------------
!      Name           Var        dVol > 0       dVol <= 0
!     --------      -------     ----------     -----------
!     Upwind          Upw       shift  -2       shift  +1
!     Donor           Don       shift  -1       shift   0
!     Downwind        Dnw       shift   0       shift  -1
!----------------------------------------------------------------------------

Real(kind=kreal), parameter :: zero(2) = [0.0_kreal, 0.0_kreal]

   ! variables at cell boundaries (0)=> left, (1)=>right
   !
   real(kind=kreal), dimension(0:1) :: Dnw, Don, Upw, correct, flux

   ! local array or array section variables
   !
   real(kind=kreal), dimension(-2:2) :: Array_l(-2:2)
   real(kind=kreal), dimension(0:1)  :: lVol_l, lUpw_l, lDnw_l, Eps_l

   ! load arrays into a thread's local memory (registers)
   !
   Array_l = halo(Array, HALO=[2,2], DIM=id)
   lVol_l  = halo(lVol , HALO=[0,1], DIM=id)
   lUpw_l  = halo(lUpw), HALO=[0,1], DIM=id)
   lDnw_l  = halo(lDnw), HALO=[0,1], DIM=id)
   Eps_l   = halo(Eps) , HALO=[0,1], DIM=id)

   Dnw = MERGE(Array_l( 0: 1), Array_l(-1:0), lVol_l)
   Don = MERGE(Array_l(-1: 0), Array_l( 0:1), lVol_l)
   Upw = MERGE(Array_l(-2:-1), Array_l( 1:2), lVol_l)

   !... Compute the adjacent cell value differences on either of side flux face
   Upw = MERGE(Don - Upw, zero, lUpw_l)
   Dnw = MERGE(Dwn - Don, zero, lDnw_l)

   !... Compute derivative correction term
   correct = MERGE(Eps, zero, Upw*Dnw > zero)

   !... Upwind with Young/van Leer 3rd order gradient limiter
   Upw = SIGN(correct, Dnw) * MIN(ABS(Upw), ABS(Dnw), Eps3*ABS(Upw) + Eps4*ABS(Dnw))

   !... Material flux at interfaces
   flux = dFlux * (Don + Upw)

   !... Update Array cell values (boundaries fixed later in set_ghosts)
   if (Maskc(i,j,k)) then
      Array(i,j,k) = TmpB * (l_Array(0) * TmpA + (flux(0) - flux(1)))
   end if

End Subroutine Advect_Basic_local
