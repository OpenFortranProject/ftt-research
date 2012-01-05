pure elemental subroutine identity(A, B)
!   !
!   ! compiler directives that will eventually be turned into syntax
!   ! 
!   !$OFP CONCURRENT :: identity
!   !$OFP intent(COPYIN)   :: A
!   !$OFP intent(COPYOUT)  :: B
!   !
!
   real, intent(in), value :: A
   real, intent(out)       :: B

   B = A

end subroutine identity
