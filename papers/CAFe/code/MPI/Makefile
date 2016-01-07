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


all : $(OBJS) poisson-multigrid_1d

poisson-multigrid_1d : $(OBJS) poisson-multigrid_1d.o
	$(FC) -o $@ $(LDFLAGS) poisson-multigrid_1d.o $(OBJS) $(LIBS)

check : poisson-multigrid_1d
	rm -f *.vti *.dat
	./poisson-multigrid_1d

check-par : poisson-multigrid_1d
	rm -f *.vti *.dat
	mpirun -np 4 poisson-multigrid_1d

clean :
	rm -rf $(OBJS) *.vti *.dat *.mod *.o poisson-multigrid_1d
