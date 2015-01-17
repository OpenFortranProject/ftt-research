Program test_halo
!=====================================================================

Use  ::  Parallel_Halo , only : Context, Parallel_Topology, Parallel_Start, Parallel_End
Use  ::  Parallel      , only : Left, Right, Bottom, Top, Front, Back
Use  ::  MPI_F08
Implicit None

!... Local Variables
!-------------------
Type(Context) :: aContext
Integer       :: ndims, dims(3)

!---------------------------------------------------------------------

Call Parallel_Start(aContext)

ndims = 2
dims  = [aContext%size/2,0,0]

Call Parallel_Topology(aContext, ndims, dims)

if (Front /= aContext%rank) STOP 'ERROR: Front not me'
if (Back  /= aContext%rank) STOP 'ERROR: Back  not me'

if (aContext%rank == 0) then
   print *, "(size,dims) =", aContext%size, dims
   print *
end if

call MPI_Barrier(MPI_COMM_WORLD)

print "(1I6,':  ',4I6)", aContext%rank, Left, Right, Bottom, Top

Call Parallel_End(aContext)

End Program test_halo
