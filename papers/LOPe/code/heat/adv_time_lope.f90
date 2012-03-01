!------------------------------------------------------------
! Proposed syntax for the concurrent procedure adv_time_lope
!------------------------------------------------------------

CONCURRENT subroutine adv_time_lope(T)
   real, intent(in out)  ::  T(0:,0:)
   HALO                  ::  T(-1:*:1,-1:*:1)
   real                  ::  lhs1, lhs2

   lhs1 = ( T(-1,0) - 2.0*T(0,0) + T(+1,0) ) / (dx*dx)
   lhs2 = ( T(0,-1) - 2.0*T(0,0) + T(0,+1) ) / (dy*dy)

   T(0,0) = (alpha * dt * (lhs2 + lhs2)) + T(0,0)

end subroutine adv_time_lope
    
