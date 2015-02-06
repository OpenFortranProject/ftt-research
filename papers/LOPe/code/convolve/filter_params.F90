#include "npad.h"

module FilterParams
   use ISO_C_BINDING

   ! WARNING, must also change in convolve.cl
   integer(c_int), parameter :: NPAD = PADDING

!  integer, parameter :: G_WIDTH  = 480
!  integer, parameter :: G_HEIGHT = 528
   integer, parameter :: G_WIDTH  = 512
   integer, parameter :: G_HEIGHT = 512

   integer(c_int), parameter :: NXP = 1 + 2*NPAD
   integer(c_int), parameter :: NYP = NXP

   integer(c_size_t), parameter :: NX   = G_WIDTH
   integer(c_size_t), parameter :: NY   = G_HEIGHT
   integer(c_size_t), parameter :: NXEX = NX + 2*NPAD
   integer(c_size_t), parameter :: NYEX = NY + 2*NPAD
   integer(c_size_t), parameter :: NXL  = 16
   integer(c_size_t), parameter :: NYL  = 16

   integer(c_size_t), parameter :: SIZE_ELEMENT = 4

end module FilterParams
