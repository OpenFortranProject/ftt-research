subroutine gauss(aug,lim,limaug,n,x,singul)
  real :: aug(lim,limaug), x(lim), temp, mult, epsil
  parameter (epsil = 1e-6)
  integer :: n, pivrow
  logical :: singul

  singul = .false.
  do 50 i=1,n
     abspiv = abs(aug(i,i))
     pivrow = i
     do 10 k=i+1,n
        if (abs(aug(k,i)) .gt. abspiv) then
           abspiv = abs(aug(k,i))
           pivrow = k
        end if
10   continue

     if (abspiv .lt. epsil) then
        singul = .true.
        return
     end if

     if (pivrow .ne. i) then
        do 20 j = 1, n+1
           temp = aug(i,j)
           aug(i,j) = aug(pivrow,j)
           aug(pivrow,j) = temp
20      continue
     end if

     do 40 j = i+1,n
        mult = -aug(j,i) / aug(i,i)
        do 30 k = i, n+1
           aug(j,k) = aug(j,k) + mult * aug(i,k)
30      continue
40   continue

50 continue

   x(n) = aug(n,n+1) / aug(n,n)
   do 70 j = n-1,1,-1
      x(j) = aug(j,n+1)
      do 60 k=j+1,n
         x(j) = x(j) - aug(j,k) * x(k)
60    continue
      x(j) = x(j) / aug(i,j)
70 continue

end subroutine gauss
