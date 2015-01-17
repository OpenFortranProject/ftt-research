Module Parallel_Halo
!=====================================================================
! Module Parallel_Halos provides types and interfaces for running
! arrays with halos in a parallel context.
!
!   ------------------------------------------------------------------
!
!   ------------------------------------------------------------------
!
! Externals :
!   MPI-3 MPI_F08 module
!   W. Weseloh, LA-CC-05-074, "Parallel Library"
!
!=====================================================================

Use            :: MPI_F08
Use            :: Parallel_Halo_Types
Use, Intrinsic :: ISO_C_BINDING
Implicit None
Save
 
Interface
   Subroutine Parallel_Start(aContext) Bind(C,name='Parallel_Start')
      Import Context
      Type(Context) :: aContext
   End Subroutine Parallel_Start

   Subroutine Parallel_End(aContext) Bind(C,name='Parallel_End')
      Import Context
      Type(Context) :: aContext
   End Subroutine Parallel_End

   Subroutine Parallel_Topology(aContext, ndims, dims) Bind(C,name='Parallel_Topology')
      Import Context
      Type(Context)                    :: aContext
      Integer, intent(in)              :: ndims
      Integer, intent(inout), optional :: dims(ndims)
   End Subroutine Parallel_Topology

   Subroutine Parallel_Topology_Defaults(aContext) Bind(C,name='Parallel_Topology_Defaults')
      Import Context
      Type(Context) :: aContext
   End Subroutine Parallel_Topology_Defaults

End Interface

End Module Parallel_Halo


Subroutine Parallel_Start(aContext) Bind(C,name='Parallel_Start')
Use  ::  Parallel_Halo  , only : Context
Use  ::  Parallel       , only : my_id, numproc
Use  ::  Parallel       , only : Parallel_Start_Base => Parallel_Start
Implicit None

Type(Context) :: aContext

Call Parallel_Start_Base

aContext%rank = my_id
aContext%size = numproc

End Subroutine Parallel_Start


Subroutine Parallel_End(aContext) Bind(C,name='Parallel_End')
Use  ::  Parallel_Halo  , only : Context
Use  ::  Parallel       , only : my_id, numproc
Use  ::  Parallel       , only : Parallel_End_Base => Parallel_End
Implicit None

Type(Context) :: aContext

Call Parallel_End_Base

End Subroutine Parallel_End


Subroutine Parallel_Topology(aContext, ndims, dims) Bind(C,name='Parallel_Topology')
Use  ::  Parallel_Halo  , only : Context
Use  ::  Parallel       , only : my_id, numproc
Use  ::  Parallel       , only : Topology
Implicit None

Type(Context)                    :: aContext
Integer, intent(in)              :: ndims
Integer, intent(inout), optional :: dims(ndims)

Call Topology(ndims, dims)

End Subroutine Parallel_Topology


Subroutine Parallel_Topology_Defaults(aContext) Bind(C,name='Parallel_Topology_Defaults')
Use  ::  Parallel_Halo  , only : Context
Use  ::  Parallel       , only : my_id, numproc
Use  ::  Parallel       , only : Topology_Defaults
Implicit None

Type(Context) :: aContext

Call Topology_Defaults

End Subroutine Parallel_Topology_Defaults
