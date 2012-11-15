program fma
  implicit none
  real :: m
  real :: a
  real :: f
  
  write (*,*) "Mass: "
  read (*,*) m
  write (*,*) "Acceleration: "
  read (*,*) a
  f = m*a
  write (*,*) "Force: "
  write (*,*) f
end program
