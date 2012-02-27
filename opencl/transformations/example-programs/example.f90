!%LOPe concurrent
subroutine example(A, n)
   integer, intent(in)     ::  n
   integer, intent(inout), dimension(:,:)  ::  A 

   ! local variable
   !
   integer  ::  i, j, k
   
   i = global_index(1)     ! "intrinsic" to return global in first dimension
   j = global_index(2)     ! "intrinsic" to return global in second dimension

   if (i /= j) then
      do k = 1, n 
         A = A + i*j + k
      end do
   end if
end subroutine example
