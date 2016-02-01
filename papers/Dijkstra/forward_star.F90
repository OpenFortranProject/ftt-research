module forward_star

contains

subroutine read_forward_star(nfs, off)
  integer, intent(in ) :: nfs
  integer, intent(out) :: off(3,nfs)
  integer :: i

  open(unit=1, file='arcList.txt')

  do i = 1, nfs
     read(1, *) off(1,i), off(2,i), off(3,i)
  end do

  close(unit=1)

end subroutine read_forward_star

end module forward_star

