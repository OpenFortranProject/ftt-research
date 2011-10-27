module types_mod
   type LIF_params
      real :: tauE, tauI, tauIB, tauVth
   end type LIF_params
   type uint4
      integer :: val1
   end type uint4
end module types_mod

pure subroutine &
LIF_update_state(time, dt, &
                 params, rnd, &
                 V, Vth, &
                 G_E, G_I, G_IB, &
                 phiExc, phiInh, phiInhB, &
                 R, activity)
   !$OFP extended elemental :: LIF_update_state
   use types_mod
   implicit none
   real, intent(in) :: time, dt
   type(LIF_params), intent(in) :: params
   type(uint4), intent(inout) :: rnd
   real, intent(inout) :: V, V_th
   real, intent(inout) :: G_E, G_I, G_IB
   real, intent(inout) :: phiExc, phiInh, phiInhB
   real, intent(inout) :: R, activity

   real :: exp_tauE, exp_tauI, exp_tauIB, exp_tauVth, exp_tauRate
   real :: dt_sec, GMAX

   ! temporary arrays
   real :: tauInf, VmemInf

   ! local param variables
   real :: tau, Vrest, VthRest, Vexc, Vinh, VinhB, deltaVth

   exp_tauE    = exp(-dt/params%tauE)
   exp_tauI    = exp(-dt/params%tauI)
   exp_tauIB   = exp(-dt/params%tauIB)
   exp_tauVth  = exp(-dt/params%tauVth)

   dt_sec = .001 * dt
   exp_tauRate = exp(-dt/params%tauRate)

   GMAX = 10.0

   !
   ! start of LIF2_update_exact_linear
   !

   ! define local param variables
   !
   tau   = params%tau
   Vexc  = params%Vexc
   Vinh  = params%Vinh
   VinhB = params%VinhB
   Vrest = params%Vrest

   VthRest  = params%VthRest
   deltaVth = params%deltaVth

   ! add noise
   !

   G_E  = phiExc  + G_E *exp_tauE
   G_I  = phiInh  + G_I *exp_tauI
   G_IB = phiInhB + G_IB*exp_tauIB
   
   !G_E  = (G_E  > GMAX) ? GMAX : G_E;
   !G_I  = (G_I  > GMAX) ? GMAX : G_I;
   !G_IB = (G_IB > GMAX) ? GMAX : G_IB;

   tauInf  = (dt/tau) * (1.0 + G_E + G_I + G_IB)
   VmemInf = (Vrest + G_E*Vexc + G_I*Vinh + G_IB*VinhB) / (1.0 + G_E + G_I + G_IB)

   V = VmemInf + (V - VmemInf)*exp(-tauInf)

   !
   ! start of LIF2_update_finish
   !

   Vth = VthRest + (Vth - VthRest)*exp_tauVth

   !
   ! start of update_f
   !

   !bool fired_flag = (l_V > l_Vth);

   !activ = fired_flag ? 1.0f             : 0.0f
   !V     = fired_flag ? Vrest            : V
   !Vth   = fired_flag ? l_Vth + deltaVth : Vth
   !G_IB  = fired_flag ? l_G_IB + 1.0f    : G_IB

   ! update average rate
   R = activ + R*exp_tauRate
   
   !
   ! These actions must be done outside of kernel
   !    1. set activity to 0 in boundary (if needed)
   !    2. update active indices
   !

   ! store local variables back to global memory
   !

   phiExc  = 0.0
   phiInh  = 0.0
   phiInhB = 0.0

end subroutine LIF_update_state
