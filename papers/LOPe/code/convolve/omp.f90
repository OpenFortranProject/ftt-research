PROGRAM VEC_ADD_DO
   use Timer_mod
   type(CPUTimer) :: timer
   real(c_double) :: cpu_time

   integer, parameter :: N = 100000
   integer, parameter :: CHUNKSIZE = 10000

   INTEGER CHUNK, I
   REAL A(N), B(N), C(N)

!  Some initializations
   do I = 1, N
      A(I) = I * 1.0
      B(I) = A(I)
   end do
   CHUNK = CHUNKSIZE
        
   C = 2*A + 3*B
   print *, C(10)   

   call init(timer)
   call start(timer)

!   do I = 1, N
!      C(I) = A(I) + B(I)
!   enddo
   C = A + B

   call stop(timer)
   cpu_time = elapsed_time(timer)

   print *, "   host time    ==   ", real(cpu_time), " msec"

   call init(timer)
   call start(timer)

!!$OMP PARALLEL SHARED(A,B,C,CHUNK) PRIVATE(I)
!$OMP PARALLEL

!!$OMP DO SCHEDULE(DYNAMIC,CHUNK)
!$OMP DO
   do I = 1, N
      C(I) = A(I) + B(I)
   end do

!$OMP END DO
!!$OMP END DO NOWAIT

!$OMP END PARALLEL

   call stop(timer)

   cpu_time = elapsed_time(timer)
   print *, "    omp time    ==   ", real(cpu_time), " msec"

end program
