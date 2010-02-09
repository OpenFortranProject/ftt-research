program mapdemo
   use MapALF

   integer, parameter :: MX = 16
   integer, parameter :: MY = 16
   integer, parameter :: MZ = 16

   real, dimension(MX,MY,MZ) :: A, B, C

   type(MapData)          :: data
   type(MapTask)          :: task
!   type(MaxwellContext)   :: context


   A = 1.0
   B = 2.0

   !
   ! initialize
   !
!   call createTaskContext(task, "spu_add_demo")
   call createDataContext(data)

!   call mapContextCreate(C_LOC(context), 128)

   ! add any scalar variables
   call addScalar(data, MX, DATA_IN)

   call addArray(data, A, DATA_IN)
   call addArray(data, B, DATA_IN)
   call addArray(data, C, DATA_OUT)

   call map(task, data)

   call destroyTaskContext(task)
   call destroyDataContext(data)

end program
