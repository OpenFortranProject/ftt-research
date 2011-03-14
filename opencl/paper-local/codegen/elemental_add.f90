pure elemental function elemental_add(a, b)
!   real :: elemental_add
   real, intent(in) :: a, b

   elemental_add = a + b   

end function elemental_add
