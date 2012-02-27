pure elemental subroutine scale(A, B, s)
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
   real, intent(in)        :: s

   B = A*s

end subroutine scale
