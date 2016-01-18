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


all : $(OBJS) poisson-multigrid_3d

poisson-multigrid_3d : $(OBJS) poisson-multigrid_3d.o
	$(FC) -o $@ $(LDFLAGS) poisson-multigrid_3d.o $(OBJS) $(LIBS)

poisson-multigrid_1d : $(OBJS) poisson-multigrid_1d.o
	$(FC) -o $@ $(LDFLAGS) poisson-multigrid_1d.o $(OBJS) $(LIBS)

poisson-multigrid : $(OBJS) poisson-multigrid.o
	$(FC) -o $@ $(LDFLAGS) poisson-multigrid.o $(OBJS) $(LIBS)

check : poisson-multigrid
	rm -f *.vti *.dat
	./poisson-multigrid

check-par : poisson-multigrid_1d
	rm -f *.vti *.dat
	mpirun -np 4 poisson-multigrid_1d

clean :
	rm -rf $(OBJS) *.vti *.dat *.mod *.o poisson-multigrid_1d poisson-multigrid_3d
