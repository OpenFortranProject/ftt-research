! +   Basic advection routine

Subroutine Advect_Basic (id, Array, Eps, dFlux, TmpA, TmpB)

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

!$ OFP kernel Advect_Basic

Use Kind_Module,   only: kint, kreal
Use Param_Module,  only: mx, my, mz

Implicit None
Save

!... Scalars:
Integer(kind=kint),  intent(in) :: id    !  advection sweep direction id

!... Arrays:
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz), intent(inout) :: Array ! advection array section
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: Eps   ! residual donor mass/volume fraction
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: dFlux ! cell face mass/volume flux
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: TmpA  !
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: TmpB  !

! ... Shift to get Donor, Upwind, and Downwind values
!----------------------------------------------------------------------------
!      Name           Var        dVol > 0       dVol <= 0
!     --------      -------     ----------     -----------
!     Upwind          Upw       shift  -2       shift  +1
!     Donor           Don       shift  -1       shift   0
!     Downwind        Dnw       shift   0       shift  -1
!----------------------------------------------------------------------------

Real(kind=kreal), parameter :: zero(2) = [0.0_kreal, 0.0_kreal]

integer :: i, j, k

DO CONCURRENT(i=0:mx, j=0:my, k=0:mz)
BLOCK

   ! variables at cell boundaries (0)=> left, (1)=>right
   !
   register &
   real(kind=kreal), dimension(0:1) :: Dnw, Don, Upw, correct, flux

   ! local array or array section variables
   !
   register &
   real(kind=kreal), dimension(-2:2) :: l_Array(-2:2)
   register &
   real(kind=kreal), dimension(0:1)  :: l_lVol, l_lUpw, l_lDnw, l_Eps

   ! load arrays into a thread's local memory (registers)
   !
   l_Array = local_section(Array, HALO=[2,2], DIM=id)
   l_lVol  = local_section(lVol , HALO=[0,1], DIM=id)
   l_lUpw  = local_section(lUpw), HALO=[0,1], DIM=id)
   l_lDnw  = local_section(lDnw), HALO=[0,1], DIM=id)
   l_Eps   = local_section(Eps) , HALO=[0,1], DIM=id)

   Dnw = MERGE(l_Array( 0: 1), l_Array(-1:0), l_lVol)
   Don = MERGE(l_Array(-1: 0), l_Array( 0:1), l_lVol)
   Upw = MERGE(l_Array(-2:-1), l_Array( 1:2), l_lVol)

   !... Compute the adjacent cell value differences on either of side flux face
   Upw = MERGE(Don - Upw, zero, l_lUpw)
   Dnw = MERGE(Dwn - Don, zero, l_lDnw)

   !... Compute derivative correction term
   correct = MERGE(Eps, zero, Upw*Dnw > zero)

   !... Upwind with Young/van Leer 3rd order gradient limiter
   Upw = SIGN(correct, Dnw) * MIN(ABS(Upw), ABS(Dnw), Eps3*ABS(Upw) + Eps4*ABS(Dnw))

   !... Material flux at interfaces
   flux = dFlux * (Don + Upw)

   !... Update Array cell values (boundaries fixed later in set_ghosts)
   if (local(Maskc))  local(Array) = TmpB * (l_Array(0) * TmpA + (flux(0) - flux(1)))

END BLOCK
END DO CONCURRENT

End Subroutine Advect_Basic_local
