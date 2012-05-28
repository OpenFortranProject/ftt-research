FC = mpifort
CC = mpicc

OBJS = parallel_mpif08.o parallel_halo_types.o parallel_halo.o

%.o : %.f90
	$(FC) -c $<

%.o : %.c
	$(CC) -c $<

all : $(OBJS) test_halos test_halos_c

test_halos : test_halos.o $(OBJS)
	$(FC) -o $@ $< $(OBJS)

test_halos_c : test_halos_fc.o test_halos_c.o $(OBJS)
	$(FC) -o $@ $< $(OBJS) test_halos_c.o

check: test_halos test_halos_c
	@echo
	@echo "Testing Fortran API.........................."
	@mpirun -np 8 test_halos
	@echo
#	@echo "Testing C       API.........................."
#	@mpirun -np 2 test_halos_c
#	@echo


# dependencies
parallel_halo.o : parallel_mpif08.o parallel_halo_types.o
test_halos.o    : parallel_halo.o

clean :
	rm -rf *.o *.mod test_halos test_halos_c
