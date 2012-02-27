module Retina
  use :: ISO_C_BINDING

type :: Retina_params
   real(C_FLOAT) :: probStim
   real(C_FLOAT) :: probBase
   real(C_FLOAT) :: beginStim
   real(C_FLOAT) :: endStim
   real(C_FLOAT) :: burstFreq          ! frequence of bursts
   real(C_FLOAT) :: burstDuration      ! duration of each burst, <=0 -> sinusoidal

   real(C_FLOAT) :: refactory_period
   real(C_FLOAT) :: abs_refactory_period
end type Retina_params

type :: UINT4
   integer(C_INT) :: s1
   integer(C_INT) :: s2
   integer(C_INT) :: s3
   integer(C_INT) :: s4
end type UINT4

interface
   function spike(time, dt, prev, stimFactor, rnd_state, params)
      use :: ISO_C_BINDING
      import :: Retina_params, UINT4
      real(C_FLOAT), intent(in)        :: time, dt, prev, stimFactor
      type(UINT4), intent(in out)      :: rnd_state
      type(Retina_params), intent(in)  :: params
      integer(C_INT)                   :: spike
   end function spike
end interface


contains

!$OFP CONCURRENT :: Retina_update_state
subroutine Retina_update_state(time, dt,                         &
                               params, rnd,                      &
                               phiExc, phiInh, activity, prevTime)
   implicit none
   real, intent(in) :: time, dt
   type(Retina_params), intent(in)           :: params
   type(UINT4),         intent(in out)       :: rnd(0:,0:,0:)
   real, intent(in out), dimension(0:,0:,0:) :: phiExc, phiInh
   real, intent(in out), dimension(0:,0:,0:) :: activity, prevTime

   ! local variables
   real :: phiDiff

   phiDiff = phiExc(0,0,0) - phiInh(0,0,0)

   ! spike is an inlined function
   activity(0,0,0) = spike(time, dt, prevTime(0,0,0), phiDiff, rnd(0,0,0), params)

   prevTime(0,0,0) = MERGE(time, prevTime(0,0,0), activity(0,0,0) > 0.0)

   phiExc(0,0,0) = 0.0
   phiInh(0,0,0) = 0.0

end subroutine Retina_update_state

end module Retina
