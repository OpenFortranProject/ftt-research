Subroutine Advect_Basic_LOPe (id, Array, EpsIn, dFluxIn, TmpAIn, TmpBIn       &
                                , Eps3In, Eps4In                              &
                                , l_UpwIn, l_DnwIn, l_dVolIn, MaskcIn         &
                                , mx, my, mz                                  &
                             )

!==============================================================================
! Description: The basic advection routine.
!
! Method: The donor, upwind, and downwind advection quantities are computed.
!         The advection is performed and put back into the "Array".
!
! Reference: PAGOSA physics Manual, LA-14425-M, August 2010, Los Alamos
!            National Laboratory, p.42-48 and p.183-193, p.197-202.
!
!==============================================================================

Implicit None
Save

Integer,          parameter :: kreal =  SELECTED_REAL_KIND(6)   ! single precsion
Integer,          parameter :: kint  =  SELECTED_INT_KIND(8)    ! integers ( 8 digits)

Real(kind=kreal), parameter :: zero  =  0.0_kreal

!... Scalars:
Integer(kind=kint), intent(in) :: id              ! advection sweep direction id
Integer(kind=kint), intent(in) :: mx, my, mz      ! size of arrays

!... Arrays:
Real(kind=kreal), dimension(0:mx,0:my,0:mz), intent(in out) :: Array    ! advection array section
Real(kind=kreal), dimension(0:mx,0:my,0:mz), intent(in)     :: EpsIn    ! residual donor mass/volume fraction
Real(kind=kreal), dimension(0:mx,0:my,0:mz), intent(in)     :: dFluxIn  ! cell face mass/volume flux
Real(kind=kreal), dimension(0:mx,0:my,0:mz), intent(in)     :: TmpAIn   ! 
Real(kind=kreal), dimension(0:mx,0:my,0:mz), intent(in)     :: TmpBIn   ! 

Real(kind=kreal), dimension(0:mx,0:my,0:mz) :: Eps3In    ! UpWind advection coefficient
Real(kind=kreal), dimension(0:mx,0:my,0:mz) :: Eps4In    ! DnWind advection coefficient

Logical,          dimension(0:mx,0:my,0:mz) :: l_UpwIn    ! true for non-zero upwind   density flux
Logical,          dimension(0:mx,0:my,0:mz) :: l_DnwIn    ! true for non-zero downwind density flux
Logical,          dimension(0:mx,0:my,0:mz) :: l_dVolIn   ! true for positive cell face volume flux
Logical,          dimension(0:mx,0:my,0:mz) :: MaskcIn    ! true if both new mat vol and mass > 0

!... Local arrays:
Real(kind=kreal), dimension(-2:2) :: dVar    ! cell value / face flux

Real(kind=kreal), dimension(0:1)  :: Eps
Real(kind=kreal), dimension(0:1)  :: Eps3
Real(kind=kreal), dimension(0:1)  :: Eps4
Real(kind=kreal), dimension(0:1)  :: dFlux

Logical,          dimension(0:1)  :: l_Upw
Logical,          dimension(0:1)  :: l_Dnw
Logical,          dimension(0:1)  :: l_dVol

!... Local scalars
Real(kind=kreal)      :: TmpA
Real(kind=kreal)      :: TmpB
Logical               :: Maskc

!------------------------------------------------------------------------------

!... temporary variables needed to remove shifts

Real(kind=kreal) :: Upw_l_1, Upw_r_1, Don_l_1, Don_r_1, Dnw_l_1, Dnw_r_1
Real(kind=kreal) :: Upw_l_2, Upw_r_2, Dnw_l_2, Dnw_r_2
Real(kind=kreal) :: Upw_l_3, Upw_r_3
Real(kind=kreal) :: dVar_l_1, dVar_r_1, dVar_l_2, dVar_r_2

!------------------------------------------------------------------------------



!... Copy array section into its own separate array
!dVar = Array(:,:,:)                                          

!dVar = lope_dim_get(Array, HALO=2, DIM=id)
!l_Upw, ...

!... Shift to get Donor, Upwind and Downwind values
!--------------------------------------------------------
!    Name      dir     Var      dVol > 0       dVol <= 0
!    --------  ---     ---      ---------      ---------
!    Upwind     l     Upw_l     shift -2       shift +1
!    Donor      l     Don_l     shift -1       shift  0
!    Downwind   l     Dnw_l     shift  0       shift -1
!
!    Upwind     r     Upw_r     shift -1       shift +2
!    Donor      r     Don_r     shift  0       shift +1
!    Downwind   r     Dnw_r     shift +1       shift  0
!--------------------------------------------------------

Upw_l_1  =  MERGE(dVar(-2), dVar(+1), l_dVol(0))
Don_l_1  =  MERGE(dVar(-1), dVar( 0), l_dVol(0))
Dnw_l_1  =  MERGE(dVar( 0), dVar(-1), l_dVol(0))

Upw_r_1  =  MERGE(dVar(-1), dVar(+2), l_dVol(1))
Don_r_1  =  MERGE(dVar( 0), dVar(+1), l_dVol(1))
Dnw_r_1  =  MERGE(dVar(+1), dVar( 0), l_dVol(1))

!... Compute the adjacent cell value differences on either side of flux face
!
Upw_l_2  =  MERGE(Don_l_1 - Upw_l_1, zero, l_Upw(0))
Dnw_l_2  =  MERGE(Dnw_l_1 - Don_l_1, zero, l_Dnw(0))

Upw_r_2  =  MERGE(Don_r_1 - Upw_r_1, zero, l_Upw(1))
Dnw_r_2  =  MERGE(Dnw_r_1 - Don_r_1, zero, l_Dnw(1))

!... Compute derivative correction term
!
dVar_l_1 =  MERGE(Eps(0), zero, Upw_l_2*Dnw_l_2 > zero) 
dVar_r_1 =  MERGE(Eps(1), zero, Upw_r_2*Dnw_r_2 > zero) 

!... Upwind with Youngs/van Leer 3rd order gradient limiter
!
Upw_l_3  = SIGN(dVar_l_1,Dnw_l_2) * MIN(ABS(Upw_l_2),ABS(Dnw_l_2),Eps3(0)*ABS(Upw_l_2) + Eps4(0)*ABS(Dnw_l_2))
Upw_r_3  = SIGN(dVar_r_1,Dnw_r_2) * MIN(ABS(Upw_r_2),ABS(Dnw_r_2),Eps3(1)*ABS(Upw_r_2) + Eps4(1)*ABS(Dnw_l_2))

!... Material flux at interface
dVar_l_2  =  dFlux(0) * (Don_l_1 + Upw_l_3)  
dVar_r_2  =  dFlux(1) * (Don_r_1 + Upw_r_3)  

!... Update Array cell values (boundaries fixed later in set_ghosts) 
if (Maskc) dVar(0) = TmpB * (dVar(0) * TmpA + (dVar_l_2 - dVar_r_2)) 

!call lope_dim_put(dVar(0), Array, HALO=2, DIM=id)

End Subroutine Advect_Basic_LOPe
