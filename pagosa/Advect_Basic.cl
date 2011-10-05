#ifdef THIS_IS_FORTRAN_HEADER

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

Use constants,     only: zero
Use Kind_Module,   only: knit, kreal
Use Param_Module,  only: mx, my, mz
Use Shift_Module,  only: global_eoshift

Implicit None
Save

! . . . Scalars:
Integer(kind=kint),  intent(in) :: id    !  advection sweep direction id

! . . . Arrays:
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz), intent(inout) :: Array ! advection array section
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: Eps   ! residual donor mass/volume fraction
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: dFlux ! cell face mass/volume flux
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: TmpA  !
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: TmpB  !

! . . . Local Arrays:
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: dVar   ! cell value / face flux
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  intent(in)    :: Tmp    ! temporary array

!----------------------------------------------------------------------------------------
#endif

// ... Copy array section into its own separate array
__global float glo_dVar = Array[k];

//... Shift to get Donor, Upwind, and Downwind values
//----------------------------------------------------------------------------
//      Name           Var        dVol > 0       dVol <= 0
//     --------      -------     ----------     -----------
//     Upwind          Upw       shift  -2       shift  +1
//     Donor           Don       shift  -1       shift   0
//     Downwind        Dnw       shift   0       shift  -1
//----------------------------------------------------------------------------

// Don = global_eoshift(dVar, SHIFT=-1, BOUNDARY=zero, DIM=id)
const int shf_Don = -1;
const int idx_Don = index_eoshift(g_dVar, shf_Don, zero, id);
float     loc_Don = Array[idx_Don];

// Upw = MERGE(global_eoshift(Don,  SHIFT=-1, BOUNDARY=zero, DIM=id),         &
//             global_eoshift(dVar, SHIFT= 1, BOUNDARY=zero, DIM=id), l_Vol)
// YUK - need to track that Don already a shifted variable and combine shifts
// YUK - make sure that DIMs are the same
const int shf_Upw1 = -1 + shf_Don;
const int idx_Upw1 = index_eoshift(glo_dVar, shf_Upw1, zero, id);
const int shf_Upw2 = 1;
const int idx_Upw2 = index_eoshift(glo_dVar, shf_Upw2, zero, id);

bool  loc_l_Vol = l_Vol[k];
float loc_Upw = (loc_l_Vol) ? glo_dVar[idx_Upw1] : glo_Var[idx_Upw2];

//Dnw = MERGE(dVar, Don, l_Upw)
bool  loc_l_Upw = l_Upw[k];
float loc_Dnw = (loc_l_Upw) ? loc_dVar : loc_Don;

//Don = MERGE(Don, dVar, l_dVol)
bool  loc_l_dVol = l_dVol[k];
float loc_Don = (loc_l_dVol) ? loc_Don : loc_dVar;

! . . . Compute the adjacent cell value differences on either side flux face
Upw = MERGE(Don - Upw, zero, l_Upw)
Dnw = MERGE(Dwn - Don, zero, l_Dnw)

! . . . Compute derivative correction term
dVar = MERGE(Eps, zero, Upw*Dnw > zero)

! . . . Upwind with Young/van Leer 3rd order gradient limiter
Upw = SIGN(dVar, Dnw) * MIN(ABS(Upw), ABS(Dnw), Eps3*ABS(Upw)  +  Eps4*ABS(Dnw))

! . . . Material flux at interface
dVar = dFlux * ( Don + Upw)

! . . . Flux at next interface
Tmp = global_eoshift(dVar, SHIFT=1, BOUNDARY=zero, DIM=id)

! . . . Update Array cell values (boundaries fixed later in set_ghosts)
Where (Maskc)  Array(:,:,:) = TmpB * (Array(:,:,:) * TmpA + (dVar - Tmp))

! . . . Unset performace diagnostic
Call F_PERF_UPDATE('advect_basic", .false.)

End Subroutine Advect_Basic
