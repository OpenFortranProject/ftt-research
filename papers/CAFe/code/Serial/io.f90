Module IO

Contains

Subroutine Textual_Output(N, U, id)
   Implicit None
   Integer, intent(in) :: N
   Real,    intent(in) :: U(-1:N+1)
   Character(len=*), intent(in) :: id

   Integer :: i, fd
   Real    :: x

   fd = 13
   open(unit=fd, file="output_" // id // ".dat")

   print *, "opened file " // "output_" // id // ".dat"
   print *, "   id is ", id

   do i = -1, N+1
     x = real(i)/N
     write(fd, "(I5, F10.3, F10.3)") i, x, U(i)
   end do


End Subroutine Textual_Output

End Module IO
