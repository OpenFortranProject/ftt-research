
!+ Advect basic hydrodynamic variables (Vofm, Dm, and Em)

Subroutine Advect_Hydro (id, m, dVolm, Vol1, Mass, Tmp1, Tmp2, Tmp3)

!=========================================================================
!  Description:
!    Advection of basic hydrodynamic variables (Vofm, Dm, and Em).  The  
!    advection of these variables is different from the advection of the other 
!    simulation variables (Sxx, Sxy, Plwkm, Bfm, Ect.).
!
!  Method: 
!    The methodology is that in a one-dimensional sweep (in the direction id), the 
!    cell value is updated by considering the flu entering ( or leaving)
!    the cell at each boundary. The governing equation is:
!
!                Variable(new) = Variable(old) + (In Left Flux - Out Right Flux)
!
!    The Advection phase, for the basic hydrodynamic variable, follows the 
!    following steps:
!
!                 Volume advection
!                 Mass     advection
!                 Energy  advection
!                 Compute the new Dm (densities) and Em (specific internals energies)
!
!    Various logical masks are created for the advection step. They are use
!    in all the various advection routines.    They are:
!
!                  l__Upw   Upwind      advection flag
!                  l_Dnw     Downwind advection flag
!                  Maskc     Mass and Volume enough to advect?
!
!    Externals:
!               Pre_Advect_Bnd__2, Set_dMass_BND, Set_Shifted_Upper_BND
!
!    History:
!    Version        Date                  Author
!    - - - - - - -     - - - - - - - - - -      - - - - - - - - - - - - - -
!     17.0             07/08/2009       Wayne Weseloh
!
!    Notes:
!         Private arrays in Advect_Module:  dMass, dVolume, Eps1, Eps2, Eps3, Eps4, l_dVol,
!                                                                      Maskc, Vol1m, Vol2m
!=========================================================================

Use Constants,             only:   zero, one
Use Hydro_Module,    only:   Dm, Em, Vofm
Use Kind_Module,       only:   kreal, kint
Us Options_Module,   only:   cutd, id_symm, zeps
Use Param_Module,   only:   mx, my, mz ,nmat
Use Shift_Module,       only:   globlal_cshift, global_eoshift

Implicit None

Save

! . . . Scalars:
Integer(kind=kint),   intent(in) : :  id     !  advection sweep direction id
Integer(kind=kint),   intent(in) : :  m     !  material  index number

! . . . Arrays 3D:
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz),               intent(in out)   : :  Mass     !    cell or vertex mass
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz),               intent(in out)   : :  Tmp1    !    after energy advc,  old mass
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz),                intent(in out)  : :  Tmp2    !    after energy advc,  reciprocal
of new mass
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)                intent(in out)   : :  Tmp3    !    after energy advc, reciprocal
of new volume
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)                intent(in)          : : Vol1       !    last Pre-advection Volume
(from-Advc)

! . . .  Arrays 4D:
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz, nmat).   intent(in)           : :  dVolm  !  mat face volume flux

! . . . Local Arrays:
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  : :  Tmp4   !  temporary array
Real(kind=kreal),  dimension(0:mx, 0:my, 0:mz)  : :  dVar     !   temporary array

!-------------------------------------------------------------------------------------------------------------------------------

! . . . Set performance diagnostic
Call F_PERF_UPDATE('advect_hydro',.true)

!-------------------------------------------------------------------------------------------------------------------------------
! . . .  VOLUME - Advect cell volume for this material
!-------------------------------------------------------------------------------------------------------------------------------

dVolume = dVolm(:,:,:,m)                                                        !    volume flux from the left w/o subscript m
Tmp2       = global_eoshift(dVolume, shift=1, BOUNDARY=zero, DIM=id)     ! volume flux from the right

Vol1m      =  Vol1 * Vofm(:,:,:,m)                                             !  old mat(m) volume, on the right and on
Tmp1       =  global_cshift(Vol1m,SHIFT=-1,DIM=id)         !  the left (assume left side is donor)

Call Pre_Advect_Bnd_2 (id, Tmp1, Tmp2)                         !  set the volume and face volume flu at the mesh boundaries

Vol2m     =  Vol1m + (dVolume - Tmp2)                               !  new mat(m) volume

Where (Vol2m < zeps)
     Vol2m = zero                                                                        !  zero if nothing significantly left of this material
EndWhere

Where (.NOT. l_dVol)
     Tmp1 = Vol1m                                                                     !  switch donor volume from right
EndWhere

! . . . Calculate the residual cell volume fraction Eps! for use in volume
!       weighted advection of other variables

Tmp3  =  ABS(dVolume)

Eps1  =  MIN(Tmp3/MAX(Tmp1,zeps), one)                      !  residual vol fraction

Where (Eps1 < zero)
     Eps1 = zero                                                                         !  limit between 1 and 0
EndWhere

Eps1 = one - Eps1

!-------------------------------------------------------------------------------------------------------------------------------
! . . .  MASS  -  Advect cell mass for this material using the volume weights.
!-------------------------------------------------------------------------------------------------------------------------------

dVar  =  Dm(:,:,:,m)                                                                   !  copy array section into its own separate array
Don  =  global_eoshift(dVar, SHIFT=-1, BOUNDARY=zero,DIM=id)    !  shift to get donor, Upwind, Downwind values

Upw  =  MERGE(global_eoshift(Don  , SHIFT=-1, BOUNDARY=zero,  DIM=id),                      &
                              global_eoshift(dVar,SHIFT= 1, BOUNDARY=zero, DIM=id), l_dVol)

Dwn  =  MERGE(dVar, Don, l_dVol)
Don  =   MERGE(Don  , dVar, l_dVol)

l_Upw  =  (Don > cutd) .AND. (Upw > cutd)                        !  is there a significant density in each adjacent cell?
l_Dnw  = ( Don > cutd) .AND. (Dnw > cutd)

Upw   =  MERGE( Don - Upw, zero, l_Upw)                       !  calculate adjacent cell value differences
Dnw  =  MERGE( Dnw - Don, zero, l_Dnw)                        !  on either side of flux face

dVar  =  MERGE(Eps1, zero, Upw*Dnw > zero)                !  compute the derivative correction term

Upw  =  SIGN(dVar, Dnw)  *  MIN(ABS(Upw), ABS(Dnw), Eps3*ABS(Upw) + Eps4*ABS(Dnw))

Tmp1  = Don * Tmp1                                                               !  save mat(m) donor cell mass for density update later

dMass  =  dVolume * (Don + Upw)                                       !  calculate the mat(m) mass flux at this interface

dMass  =  SIGN(MIN(ABS(dMass), Tmp1), dMass)          !  limit flux to the original donor cell mass present

Call Set_dMass_BND  (id)                                                    !  set the cell face mass flux at each boundary face

Mass  =  Mass + dMass                                                          !  update total mass fluxed (used in the subroutine Advm)

! . . . Calculate the residual cell mass fraction Eps2 for the use in mass
!        weighted  advection of other variables

Tmp3  = ABS(dMass)

Eps2  =  MIN(Tmp3/MAX(Tmp1,zeps), one)                       !  residual mass fraction

Where ( Eps2 < zeps)
     Eps2  = zero                                                                         !  limit between 1 and 0 
EndWhere

Eps2  = one - Eps2

!-------------------------------------------------------------------------------------------------------------------------------
! . . . Note:  at this point in the routine the mat(m) interface mass flux dMass 
!       (and  its sum Mass)  are defined,  as are the volume flux dVolume, residual 
!       volume and mass fractions Eps1 and Eps2, and the cell quantities Vol1m
!       (old volume),  Vol2m (new volume), and Tmp1 (donor cell mass).
!-------------------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------------------
! . . .  ENERGY  -  Advect the cell energy for this material using the mass weights.
!-------------------------------------------------------------------------------------------------------------------------------

dVar  =  Em( : , : , : , m)                                                              !  copy array section into its own separate array
Don  =  global_eoshift(dVar, SHIFT=-1, BOUNDARY=zero, DIM=id)     !  shift to get Dono, Upwind,  Downwind values

Upw  = MERGE(global-eoshift(Don, SHIFT=-1, BOUNDARY=zero, DIM=id),                        &
                             global-eoshift(dVar, SHIFT= 1, BOUNDARY=zero, DIM=id), l_dVol)
Dnw  = MERGE(dVar, Don, l_dVol)
Don   = MERGE(Don, dVar, l_dVol)

Upw  =  MERGE(Don  -  Upw, zero, l_Upw)                          !  calculate adjacent cell value differences
Dnw  =  MERGE(Dnw  -  Don, zero, l_Dnw)                          !  on either side of flux face

dVar  =  MERGE(Eps2, zero, Upw*Dnw > zero)                   !  compute derivative correction term

Upw  =  SIGN(dVar, Dnw) * MIN(ABS(Upw), ABS(Dnw), Eps3*ABS(Upw)  +  Eps4*ABS(Dnw))

dVar  =  dMass *  (Don + Upw)                                                 !  mat(m)  "variable"  flux at this interface

!-------------------------------------------------------------------------------------------------------------------------------
! . . . Note:  at this point, dVar is the interface change in material (m) energy
!-------------------------------------------------------------------------------------------------------------------------------

! . . . Update density and energy if new volume and mass significant.
!       Otherwise,  leave them as they were in the last phase.

Tmp2  = global-eoshift(dMass,SHIFT=1, BOUNDARY=zero, DIM=id)

Call set_Shifted_Upper_BND (id, Tmp2)                                !  set the shifted dMass (Tmp2) at the upper boundary

Tmp3  =  global_eoshift(dVar, SHIFT=1, BOUNDARY=zero, DIM=id)

Tmp1  = Dm( : , : , : , m) * Vol1m                                                 !  old mat(m) cell mass
Tmp4  = Tmp1                                +  (dMass - Tmp2)               !  new Mat(m) cell mass                = old + (in left - out right)
dVar    = Em ( : , : , : , m) * Tmp1  +  (dVar    -  Tmp3)              !  new Mat(m) cell energy*mass  = old + (in left - out right)

Maskc  =  (Tmp4 > zeps)  .AND.  (Vol2m  > zeps)                   !  significant new cell mass and volume?

! . . . Finish calculating new cell density and energy
Tmp2  =  zero
Tmp3  =  zero

Where (Maskc)                                                                              !  new vol and mass < 0?
     Tmp2  =  one  /  Tmp4                                                             !  recip new mat(m) cell mass
     Em( : , : , : , m)  = dVar * Tmp2                                               !  new mat(m) cell energy
     Tmp3  =  one  /  Vol2m                                                            !  recip new mat(m) cell volume
     Dm( : , : , : , m)  = Tmp4 * Tmp3                                             !  new mat(m) cell density 
EndWhere

! . . . Unset performance diagnostic 
Call F_PERF_UPDATE('advect_hydro' , . false)

End Subroutine Advect_Hydro
