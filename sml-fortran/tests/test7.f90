function bar(i)
  integer :: i,bar

  bar=(i+1)
end function bar

program foo
  integer :: x,y

  x = 14

  y = bar(x)
end program foo
