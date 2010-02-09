program test30
  integer, dimension(10) :: x

  where (x > 5)
    x = 4
    x = 6
  elsewhere
    x = 3
    x = 5
  end where
end program test30
