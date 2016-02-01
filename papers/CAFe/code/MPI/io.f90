Module IO

Contains

Subroutine Textual_Output_1D(rank, np, N, U, id)
   Implicit None
   Integer, intent(in) :: N, rank, np
   Real,    intent(in) :: U(-1:N+1)
   Character(len=*), intent(in) :: id

   Integer :: i, fd
   Real    :: x
   Character(len=3) :: rid

   write(rid,'(I3.3)') rank
   fd = 13
   open(unit=fd, file="output_" // id // "_" // rid //".dat")

   print *, "opened file " // "output_" // id // "_" // rid // ".dat"
   print *, "   id is ", id
   print *, "rank is ", rid

   do i = -1, N+1
     x = real(i+N*rank)/(N*np)
     write(fd, "(I5, F10.3, F10.3)") i+N*rank, x, U(i)
   end do

End Subroutine Textual_Output_1D

Subroutine Textual_Output_3D(N,M,L, U, id)
   Implicit None
   Integer, intent(in) :: N,M,L
   Real,    intent(in) :: U(-1:N+1,-1:M+1,-1:L+1)
   Character(len=*), intent(in) :: id

   Integer :: i, j, k, fd
   Real    :: x

   fd = 13
   open(unit=fd, file="output_" // id // ".dat")

   print *, "opened file " // "output_" // id // ".dat"
   print *, "   id is ", id

   !! Pick points near middle for j and k
   !
   j = M/2
   k = L/2
   do i = -1, N+1
     x = real(i)/N
     write(fd, "(I5, F10.3, F10.3)") i, x, U(i,j,k)
   end do

End Subroutine Textual_Output_3D

End Module IO
