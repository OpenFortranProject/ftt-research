   PROGRAM NONBLOCK_DO
   INTEGER :: I, N, TOTAL
   INTEGER, DIMENSION(10) :: X

   DO 20 I = 1, N             ! Nonblock DO
20 TOTAL = TOTAL + X(I)       ! NOTE: this is obsoleted code

   PRINT *,TOTAL
   END
