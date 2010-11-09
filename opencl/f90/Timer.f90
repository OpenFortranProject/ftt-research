module Timer_mod

   use, intrinsic :: ISO_C_BINDING

   type :: MachTimer
      integer :: rank
      integer(c_int64_t) :: mach_start, mach_end, mach_elapsed
!      real(c_double) :: mach_start, mach_end, mach_elapsed
!   contains
!      procedure, pass(this) :: init
!      procedure, pass(this) :: start
!      procedure, pass(this) :: stop
!      procedure, pass(this) :: elapsed_time
   end type MachTimer

   interface
      function mach_absolute_time() result(time) bind(C,name="mach_absolute_time")
         use, intrinsic :: ISO_C_BINDING
         integer(c_int64_t) :: time
      end function

      function print_elapsed_time(mach_elapsed) &
               result(time) bind(C,name="print_elapsed_time")
         use, intrinsic :: ISO_C_BINDING
         integer(c_int64_t), value :: mach_elapsed
         real(c_double) :: time
      end function

   end interface

contains

   subroutine init_timer(this)
      implicit none
!      class(MachTimer) :: this
      type(MachTimer) :: this
      this%rank = 0
      this%mach_start   = mach_absolute_time()
      this%mach_end     = this%mach_start
      this%mach_elapsed = 0
   end subroutine init_timer

   subroutine start(this)
      implicit none
!      class(MachTimer) :: this
      type(MachTimer) :: this
      this%mach_start = mach_absolute_time()
   end subroutine start

   subroutine stop(this)
      implicit none
!      class(MachTimer) :: this
      type(MachTimer) :: this
      this%mach_end = mach_absolute_time();
      this%mach_elapsed = this%mach_elapsed + this%mach_end - this%mach_start
   end subroutine stop

   subroutine elapsed_time(this)
      implicit none
!      class(MachTimer) :: this
      type(MachTimer) :: this
      real(c_double) :: time
      time = print_elapsed_time(this%mach_elapsed)
   end subroutine elapsed_time

end module Timer_mod
