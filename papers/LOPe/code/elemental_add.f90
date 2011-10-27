!
! Eventual form of high-level representation.  This will first be turned
! into syntax for OFP and ROSE and then, hopefully it can make its way
! into the Fortran standard.
!
! pure elemental CONCURRENt subroutine elemental_add(A, B, C)
!    TYPE(*), intent(COPYIN)  :: A, B
!    TYPE(*), intent(COPYOUT) :: C
!
!   C = A + B   
!
!end subroutine elemental_add


!
! Note that this function is dimensionless.  The function parameters are scalars
! but the compiler will generate code for arrays of a particular dimension based
! on the actual parameters at the calling site.  Also note that the parameters
! are typeless (see TYPE(*) above); the compiler will create an instance of
! function according to the called type.
!
pure elemental subroutine elemental_add(A, B, C, s)
!   !
!   ! compiler directives that will eventually be turned into syntax
!   ! 
!   !$OFP CONCURRENT :: elemental_add
!   !$OFP intent(COPYIN)   :: A, B
!   !$OFP intent(COPYOUT)  :: C
!   !
!
   real, intent(in), value :: A, B, s
   real, intent(out) :: C

   C = A + s*B   

end subroutine elemental_add
