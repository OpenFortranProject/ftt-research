CC = cc
FC = gfortran

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
	poisson-multigrid

clean :
	rm -rf $(OBJS) *.vti *.dat *.mod *.o poisson-multigrid
