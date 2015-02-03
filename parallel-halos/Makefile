FC = mpifort
CC = mpicc

FC_FLAGS = -g

#
# This has problems with Intel compiler on linux
#
# LK_FLAGS = -Wl,-no_pie

OBJS = parallel_mpif08.o parallel_halo_types.o parallel_halo.o

%.o : %.f90
	$(FC) -c $< $(FC_FLAGS)

%.o : %.c
	$(CC) -c $<

all : $(OBJS) test_halos test_halos_c libhalos.a

test_halos : test_halos.o $(OBJS)
	$(FC) $(LK_FLAGS) -o $@ $< $(OBJS)

test_halos_c : test_halos_fc.o test_halos_c.o $(OBJS)
	$(FC) -o $@ $< $(OBJS) test_halos_c.o

libhalos.a: $(OBJS)
	ar -r libhalos.a $(OBJS)
	ranlib libhalos.a

check: test_halos test_halos_c
	@echo
	@echo "Testing Fortran API.........................."
	@mpirun -np 4 test_halos
	@echo
#	@echo "Testing C       API.........................."
#	@mpirun -np 2 test_halos_c
#	@echo


# dependencies
parallel_halo.o : parallel_mpif08.o parallel_halo_types.o
test_halos.o    : parallel_halo.o

clean :
	rm -f *.o *.mod test_halos test_halos_c
	rm -f libhalos.a
