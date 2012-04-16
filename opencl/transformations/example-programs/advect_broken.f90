Subroutine Advect_Basic_LOPe ()
Implicit None
Save
Integer,          parameter :: kreal =  SELECTED_REAL_KIND(6)   ! single
Real(kind=kreal), dimension(0:1)  :: Eps3
Real(kind=kreal) :: Upw_l_3
Upw_l_3  = Eps3(0)
End Subroutine Advect_Basic_LOPe
