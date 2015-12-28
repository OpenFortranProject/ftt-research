CC = cc
FC = mpif90

OPT_FLAG = -g

CFLAGS  = $(OPT_FLAG)
FFLAGS  = $(OPT_FLAG)
LDFLAGS = 

OBJS = multigrid.o io.o error.o # jacobi.o vtk.o vtk_output.o
LIBS = 

%.o : %.c
	$(CC) -c $< $(CFLAGS)

%.o : %.f90
	$(FC) -c $< $(FFLAGS)

%.o : %.F90
	$(FC) -c $< $(FFLAGS)


all : $(OBJS) poisson-multigrid

poisson-multigrid : $(OBJS) poisson-multigrid.o
	$(FC) -o $@ $(LDFLAGS) poisson-multigrid.o $(OBJS) $(LIBS)

check : poisson-multigrid
	rm -f *.vti *.dat
	./poisson-multigrid

check-par : poisson-multigrid
	rm -f *.vti *.dat
	mpirun -np 2 poisson-multigrid

clean :
	rm -rf $(OBJS) *.vti *.dat *.mod *.o poisson-multigrid
