module WeightParams
   use ISO_C_BINDING

   ! WARNING, must also change in recv_synaptic_input.cl
   integer(c_int), parameter :: NPAD = 3

   integer, parameter :: G_WIDTH = 128

   integer(c_int), parameter :: NXP = 1 + 2*NPAD
   integer(c_int), parameter :: NYP = NXP

   integer(c_size_t), parameter :: NX   = G_WIDTH
   integer(c_size_t), parameter :: NY   = G_WIDTH
   integer(c_size_t), parameter :: NXEX = NX + 2*NPAD
   integer(c_size_t), parameter :: NYEX = NY + 2*NPAD
   integer(c_size_t), parameter :: NXL  = 16
   integer(c_size_t), parameter :: NYL  = 16

   integer(c_size_t), parameter :: SIZE_ELEMENT = 4

end module WeightParams
