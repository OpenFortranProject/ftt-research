Module IO

Contains

Subroutine Textual_Output(rank, N, U, id)
   Implicit None
   Integer, intent(in) :: N, rank
   Real,    intent(in) :: U(-1:N+1)
   Character(len=*), intent(in) :: id

   Integer :: i, fd
   Real    :: x
   Character(len=3) :: rid
  
   write (rid,'(I3.3)') rank
   fd = 13
   open(unit=fd, file="output_" // id // "_" // rid // ".dat")

   print *, "opened file " // "output_" // id // "_" // rid // ".dat"
   print *, "   id is ", id
   print *, "rank is ", rid

   do i = -1, N+1
     x = real(i)/N
     write(fd, "(I5, F10.3, F10.3)") i+N*rank, x, U(i)
   end do


End Subroutine Textual_Output

End Module IO
