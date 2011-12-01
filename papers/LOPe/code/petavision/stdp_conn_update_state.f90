!
! This function updates the Minus array and operates overs
! all post synaptic neurons in the extended layer (includes halo region)
!
pure concurrent subroutine stdp_conn_update_state(time, dt, ampLTD, tauLTD, PostA, M)
   real, intent(in)    ::  time, dt, ampLTD, tauLTD
   real, intent(in)    ::  PreA(0:,0:)
   real, intent(inout) ::     M(0:,0:)

   ! local variables
   !
   real                           ::  decay

   decay = exp(-dt/tauLTD)

   M(0,0) = decay*M(0,0) - ampLTD*PostA(0,0)

end subroutine stdp_conn_update_state

