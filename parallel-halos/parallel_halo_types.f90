Module Parallel_Halo_Types
!=====================================================================
! Module Parallel_Halo_Types defines types (C structs) for halo usage
! in a parallel context.
!
!=====================================================================

Use, Intrinsic :: ISO_C_BINDING
Use            :: MPI_F08
Implicit None
Save
 
Type, Bind(C) :: Context
   !... Parallel context
   !--------------------
   Integer(C_INT) :: rank          ! id of this process
   Integer(C_INT) :: size          ! number of participating processes
   Type(MPI_Comm) :: comm          ! communicator for this context
End Type Context

Type, Bind(C) :: Halo
   !... Halo size
   !-------------
   Integer(C_INT) :: hlx, hrx          ! halo size (x-direction)
   Integer(C_INT) :: hly, hry          ! halo size (y-direction)
   Integer(C_INT) :: hlz, hrz          ! halo size (z-direction)
End Type Halo

Type, Bind(C) :: Topology
   !... Cartesian topology
   !----------------------
   Integer(C_INT) :: Left, Right       ! Neighbor ranks (x-direction)
   Integer(C_INT) :: Bottom, Top       ! Neighbor ranks (y-direction)
   Integer(C_INT) :: Front, Back       ! Neighbor ranks (z-direction)

   !... Cartesian decomposition
   !---------------------------
   Integer(C_INT) :: npex=0            ! Number of processors X direction
   Integer(C_INT) :: npey=0            ! Number of processors Y direction
   Integer(C_INT) :: npez=0            ! Number of processors Z direction
End Type Topology

End Module Parallel_Halo_Types
