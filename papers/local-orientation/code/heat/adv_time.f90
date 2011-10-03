!------------------------
! Proposed syntax for the concurrent function adv_time
!------------------------

pure elemental function adv_time(T)
   real, EXTEND, intent(in)  :: T(-1:1,-1:1)
   real :: adv_time
   real :: lhs1, lhs2

   lhs1 = ( T(-1,0) - 2.0*T(0,0) + T(+1,0) ) / (dx*dx)
   lhs2 = ( T(0,-1) - 2.0*T(0,0) + T(0,+1) ) / (dy*dy)

   adv_time = (alpha * dt * (lhs2 + lhs2)) + T(0,0)

 end function adv_time
    
