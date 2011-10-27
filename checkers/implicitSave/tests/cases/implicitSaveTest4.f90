function f()
  integer :: x = 4
  print *, x
  x = x + 1
  f = 0
end

program main
  integer :: x
  print *, "Testing f"
  x = f()
  x = f()
  x = f()
end program main
