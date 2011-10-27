module file_io
   use :: iso_c_binding

interface
   integer(C_INT) function image_size(filename, nx, ny) &
      bind(c,name="image_size")
      use :: iso_c_binding
      character(C_CHAR), intent(in) :: filename(*)
      integer(C_INT), intent(out) :: nx, ny
    end function image_size

    integer(C_INT) function print_header(filename) &
       bind(c,name="print_header")
       use :: iso_c_binding
       character(C_CHAR), intent(in) :: filename(*)
     end function print_header

    integer(C_INT) function read_image(filename, image) &
       bind(c,name="read_image")
       use :: iso_c_binding
       character(C_CHAR), intent(in) :: filename(*)
       integer(C_CHAR), intent(out) :: image(*)
    end function read_image
  
    subroutine read_image_file(filename, nx, ny, image) &
       bind(c,name="read_image_file")
       use :: iso_c_binding
       character(C_CHAR), intent(in) :: filename(*)
       real(C_FLOAT), intent(out) :: image(*)
       integer, value, intent(in) :: nx, ny
    end subroutine read_image_file

    subroutine write_image_file(filename, nx, ny, image) &
       bind(c,name="write_image_file")
       use :: iso_c_binding
       character(C_CHAR), intent(in) :: filename(*)
       real(C_FLOAT), intent(in) :: image(*)
       integer, value, intent(in) :: nx, ny
    end subroutine write_image_file
end interface

end module file_io

!program main
!  use file_io
!  implicit none
!
!  integer(C_INT) :: size, nx, ny
!  integer(C_CHAR), allocatable :: image(:)
!
!  size = image_size("lena-sjooblom.pvp", nx, ny)
!  print *, " image size is", size, nx, ny
!
!  allocate(image(size))
!
!  size = read_image("lena-sjooblom.pvp", image)
!  print *, " read size is ", size
!
!  print *, image(1:10)
!
!end program
