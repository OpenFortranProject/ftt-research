module dijkstra
  implicit none

  real, parameter :: DIST_FACTOR = 1.0

contains

!-----------------------------------------------------------------------
! Sweep over the grid while updating the travel time
!-----------------------------------------------------------------------
subroutine sweep(nx,ny,nz, nfs, U, TT, Offset, Changed)
   implicit none
   integer, intent(in)    :: nx, ny, nz, nfs
   real,    intent(in)    ::  U(nx,ny,nz)
   real,    intent(inout) :: TT(nx,ny,nz)
   integer, intent(in)    :: Offset(3,nfs)
   integer, intent(inout) :: Changed(nx,ny,nz)
   
   !--- local variables ---
   !
   integer :: i, j, k, l, is, js, ks, chg
   real    :: t, t0, tt_min, u0, dist, delay
   
   !! check travel time at each node
   !
   do k = 1, nz
      do j = 1, ny
         do i = 1, nx
            chg = 0                  ! initially there are no changes
            u0 =  U(i,j,k)
            t0 = TT(i,j,k)
            tt_min = t0
            ! check each node in forward star
            do l = 1, nfs
               is = i + Offset(1,l);  if (is < 1) goto 10;  if (is > nx) goto 10
               js = j + Offset(2,l);  if (js < 1) goto 10;  if (js > ny) goto 10
               ks = k + Offset(3,l);  if (ks < 1) goto 10;  if (ks > nz) goto 10

               dist = DIST_FACTOR*sqrt( real((is-i)*(is-i) + (js-j)*(js-j) + (ks-k)*(ks-k)) )
               delay = 0.5*(u0 + U(is,js,ks))*dist

               t = TT(is,js,ks) + delay
               if (t < t0) then       ! update travel time
                  chg = 1
                  t0 = t
                  tt_min = t
               end if

10             continue
            end do
            Changed(i,j,k) = chg
            TT(i,j,k) = tt_min
         end do
      end do
   end do

end subroutine sweep

end module dijkstra
