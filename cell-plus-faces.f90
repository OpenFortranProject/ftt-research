module concurrent_mod
contains

   concurrent subroutine map_to_x_faces(H, Hx)
      ! do concurrent (0:nx) so halo on right
      real, halo(0:*:1,0) :: H(:,:)
      real                :: Hx(0:,0:)
      integer, parameter  :: left = 0, right = 1

      Hx(0,0) = ( H(left,0) + H(right,0) ) / 2.0

   end subroutine map_to_x_faces

   concurrent subroutine map_to_y_faces(H, Hy)
      ! do concurrent (0:ny) so halo on top
      real, halo(0,0:*:1) :: H(:,:)
      real                :: Hy(0:,0:)
      integer, parameter  :: bottom = 0, top = 1

      Hy(0,0) = ( H(0,bottom) + H(0,top) ) / 2.0

   end subroutine map_to_y_faces

end module concurrent_mod

program cell_plux_faces
   use :: concurrent_mod
   ! numCells = NX+2
   ! numFaces = NX+1
   real, halo(1:*:1,1:*:1) :: H(NX,NY)
   real                    :: Hx(0:NX,NY), Hy(NX,0:NY)
   integer :: i, j

   ! interpolate (map) to faces (for all faces)
   !

   do concurrent(i=0:NX,j=1:NY)
      call map_to_x_faces(H(i,j), Hx(i,j)) ! reads from H(dim=1) halos
   end do
   do concurrent(i=1:NX,j=0:NY)
      call map_to_y_faces(H(i,j), Hy(i,j)) ! reads from H(dim=2) halos
   end do

end program
