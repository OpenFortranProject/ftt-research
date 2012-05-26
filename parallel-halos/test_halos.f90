Program test_halo
!=====================================================================

Use  ::  Parallel_Halo
Implicit None

Type(Context) :: aContext

!---------------------------------------------------------------------

Call Parallel_Start(aContext)

print *, aContext%rank, aContext%size

Call Parallel_Topology(aContext)

Call Parallel_End(aContext)

End Program test_halo
