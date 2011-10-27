module CFD

  type :: snode
    real :: val
  end type snode

  type :: node
    real :: val
    class(node), pointer :: child(:)
  end type node

  type, extends(snode) :: cnode
    class(node), pointer :: child(:)
  end type cnode

contains

  real function myval(this)
     class(cnode), intent(in) :: this
     myval = this%val

     select type(this)
!     type is (snode)
     type is (cnode)
     end select

  end function myval

end module CFD

pure elemental function value(a)
   use CFD
   real :: value
   class(node), intent(in) :: a

   if (associated(a%child)) then
      value = sum(a%child(1:4)%val) / 4.0
   else
      value = a%val
   end if
   

end function value

