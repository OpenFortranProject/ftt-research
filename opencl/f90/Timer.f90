module Timer_mod

   use, intrinsic :: ISO_C_BINDING

   type :: CPUTimer
      integer :: rank
      integer(c_int64_t) :: time_start, time_end, time_elapsed
!      real(c_double) :: time_start, time_end, time_elapsed
!   contains
!      procedure, pass(this) :: init
!      procedure, pass(this) :: start
!      procedure, pass(this) :: stop
!      procedure, pass(this) :: elapsed_time
   end type CPUTimer

   interface
      function get_cpu_time() result(time) bind(C,name="get_cpu_time")
         use, intrinsic :: ISO_C_BINDING
         integer(c_int64_t) :: time
      end function

      function print_elapsed_time(time_elapsed) &
               result(time) bind(C,name="print_elapsed_time")
         use, intrinsic :: ISO_C_BINDING
         integer(c_int64_t), value :: time_elapsed
         real(c_double) :: time
      end function

   end interface

contains

   subroutine init_timer(this)
      implicit none
!      class(CPUTimer) :: this
      type(CPUTimer) :: this
      this%rank = 0
      this%time_start   = get_cpu_time()
      this%time_end     = this%time_start
      this%time_elapsed = 0
   end subroutine init_timer

   subroutine start(this)
      implicit none
!      class(CPUTimer) :: this
      type(CPUTimer) :: this
      this%time_start = get_cpu_time()
   end subroutine start

   subroutine stop(this)
      implicit none
!      class(CPUTimer) :: this
      type(CPUTimer) :: this
      this%time_end = get_cpu_time();
      this%time_elapsed = this%time_elapsed + this%time_end - this%time_start
   end subroutine stop

   subroutine elapsed_time(this)
      implicit none
!      class(CPUTimer) :: this
      type(CPUTimer) :: this
      real(c_double) :: time
      time = print_elapsed_time(this%time_elapsed)
   end subroutine elapsed_time

end module Timer_mod
