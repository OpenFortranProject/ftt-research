pure subroutine elemental_add(A, B, C)
   real, intent(in)  :: A, B
   real, intent(out) :: C

   !
   ! Add compiler directives that will be turned into syntax
   ! 
   !$OFP thread elemental :: elemental_add
   !$OFP intent(COPYIN)   :: A, B
   !$OFP intent(COPYOUT)  :: C
   !

   C = A + B   

end subroutine elemental_add
