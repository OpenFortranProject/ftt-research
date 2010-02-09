subroutine matmul(mat1, mat2, prod, limit, m, n, p, q, match)

      integer :: limit, m, n, p, q
      real :: mat1(limit,limit), mat2(limit,limit), prod(limit,limit)
      integer :: i,j,k
      real :: sum
      logical :: match

      if (n .eq. p) then
         match = .true.
         do 30 i=1,m
            do 20 j=1,q
               sum = 0
               do 10 k=1,n
                  sum = sum + mat1(i,k) * mat2(k,j)
 10            continue
               prod(i,j) = sum
 20         continue
 30      continue
      else
         match = .false.
      end if
end subroutine matmul
