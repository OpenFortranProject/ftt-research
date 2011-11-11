program test

integer :: x = 3

if (.false.) then
  x = x + 1
else
  print *,x
end if

end program
