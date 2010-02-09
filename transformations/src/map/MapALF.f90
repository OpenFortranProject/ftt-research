module MapALF
  use, intrinsic :: iso_c_binding

  integer, parameter :: DATA_IN    = 1
  integer, parameter :: DATA_OUT   = 2
  integer, parameter :: DATA_INOUT = 3

  !
  ! move this to separate module
  !
  type MaxwellContext ! BIND(C)
    real(C_FLOAT) :: dt
  end type MaxwellContext


  type MapData ! BIND(C)
    type(C_PTR) :: cptr
  end type MapData

  type MapTask ! BIND(C)
    type(C_PTR) :: cptr
  end type MapTask

  !
  ! Map interfaces
  !
  interface addArray
    module procedure addArray_R3D

    subroutine map_add_data(data, ptr, size, intent) BIND(C, name="map_add_data")
      use, intrinsic :: iso_c_binding
      import MapData
      type(MapData), value  :: data
      type(C_PTR), value    :: ptr
      integer(C_INT), value :: size
      integer(C_INT), value :: intent
    end subroutine map_add_data
  end interface

  interface
    subroutine createDataContext(data) BIND(C, name="map_create_data_context")
      use, intrinsic :: iso_c_binding
      import MapData
      type(MapData)  :: data
    end subroutine createDataContext
  end interface

  interface
    subroutine createTaskContext(task, kernel) BIND(C, name="map_create_task_context")
      use, intrinsic :: iso_c_binding
      import MapTask
      type(MapTask)  :: task
      type(C_FUNPTR) :: kernel
    end subroutine createTaskContext
  end interface

  interface
    subroutine mapContextCreate(context, size) BIND(C)
      use, intrinsic :: iso_c_binding
      type(C_PTR)    :: context
      integer(C_INT) :: size
    end subroutine mapContextCreate
  end interface

  interface
    subroutine map(task, data) BIND(C, name="map")
      use, intrinsic :: iso_c_binding
      import MapTask, MapData
      type(MapTask), value :: task
      type(MapData), value :: data
!      type(C_PTR) :: context
    end subroutine map
  end interface

  !
  ! SPU kernel interface
  !
  interface
    function alf_comp_kernel(context, parms, &
                             inputs, outputs, &
                             current_count, total_count) BIND(C)
      use, intrinsic :: iso_c_binding
      type(C_PTR) :: context, parms, inputs, outputs
      integer(C_INT) :: current_count, total_count, alf_comp_kernel
    end function alf_comp_kernel
  end interface

contains

  subroutine addArray_R3D(data, A, intent)
    type(MapData) :: data
    real :: A(:,:,:)
    integer, intent(in) :: intent

    integer :: size(3)

    size = 1 + UBOUND(A) - LBOUND(A)

    print *, "mapDataInsert: size =", size, product(size)

    call addArray(data, C_LOC(A), product(size), intent)

  end subroutine addArray_R3D

end module MapALF
