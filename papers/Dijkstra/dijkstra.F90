module dijkstra
  implicit none

contains

!-----------------------------------------------------------------------
! Relax the vertex given by the indices (i0,j0,k0) using loops
!-----------------------------------------------------------------------
pure subroutine relax(nx,ny,nz, nfs, U, TT, Offset, Changed)
   implicit none
   integer, intent(in)    :: nx, ny, nz, nfs
   real,    intent(in)    ::  U(nx,ny,nz)
   real,    intent(inout) :: TT(nx,ny,nz)
   integer, intent(in)    :: Offset(3,nfs)
   integer, intent(inout) :: Changed(nx,ny,nz)
   
   !--- local variables ---
   !
   integer :: i, j, k, l, is, js, ks
   real    :: t, t0, u0, dist
   
   !! initially there are no changes
   !
   Changed = 0

   !! relax travel time at each node
   !
   do k = 1, nz
      do j = 1, ny
         do i = 1, nx
            u0 =  U(i,j,k)
            t0 = TT(i,j,k)
            ! check each node in forward star
            do l = 1, nfs
               is = i + Offset(1,l);  if (is < 1) goto 10;  if (is > nx) goto 10
               js = j + Offset(2,l);  if (js < 1) goto 10;  if (js > ny) goto 10
               ks = k + Offset(3,l);  if (ks < 1) goto 10;  if (ks > nz) goto 10

               dist = 10*sqrt( real(is*is + js*js + ks*ks) )

               t = TT(is,js,ks) + 0.5*(u0 + U(is,js,ks))*dist
               if (t < t0) then       ! update travel time
                              t0 = t
                       TT(i,j,k) = t
                  Changed(i,j,k) = 1
               end if

10             continue
            end do
         end do
      end do
   end do

end subroutine relax

end module dijkstra
