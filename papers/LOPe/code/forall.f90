program test_forall
   implicit none
   integer, parameter :: ng = 2
   integer, parameter :: is = 1, ie = 10
   integer, parameter :: km = 100

   real, dimension(is-ng:ie+ng,km) :: delp
   real, dimension(is:ie,km) :: dm

   integer :: k, i

   forall(k=1:km, i=is:ie)
      dm(i,k) = delp(i,k-1)
   end forall

end program
