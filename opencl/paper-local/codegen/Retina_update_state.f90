pure subroutine &
Retina_update_state(time, dt, nx, ny, nf, nb, params, &
                    phiExc, phiInh, activity)
   !$OFP extended elemental :: Retina_update_state
   implicit none
   real, intent(in) :: time, dt
   integer, intent(in) :: nx, ny, nf, nb
   type(Retina_params), intent(in) :: params
   type(C_PTR) :: params1
   real, intent(inout) :: phiExc, phiInh, activity

end subroutine Retina_update_state
