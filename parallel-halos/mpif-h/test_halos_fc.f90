program test_halos_fc
  interface
     subroutine test_halos_c() bind(C, name="test_halos_c")
     end subroutine test_halos_c
  end interface

  call test_halos_c

end program test_halos_fc
