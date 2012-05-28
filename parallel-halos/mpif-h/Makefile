FC = mpifort
CC = mpicc

OBJS = parallel.o parallel_halo_types.o parallel_halo.o

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
#
parallel_halo_types.o : ../parallel_halo_types.f90
	$(FC) -c -o $@ $<

parallel_halo.o : ../parallel_halo.f90 parallel.o parallel_halo_types.o
	$(FC) -c -o $@ $<

test_halos.o : ../test_halos.f90 parallel_halo.o
	$(FC) -c -o $@ $<

test_halos_fc.o : ../test_halos_fc.f90
	$(FC) -c -o $@ $<

test_halos_c.o : ../test_halos_c.c
	$(CC) -c -o $@ $<

clean :
	rm -rf *.o *.mod test_halos test_halos_c
