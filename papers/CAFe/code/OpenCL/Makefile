CC = gcc
FC = ifort
# FC = gfortran

OPT_FLAG = -g

CFLAGS  = $(OPT_FLAG)
FFLAGS  = $(OPT_FLAG)
LDFLAGS = 

OBJS = 
LIBS = 

%.o : %.c
	$(CC) -c $< $(CFLAGS)

%.o : %.f90
	$(FC) -c $< $(FFLAGS)

%.o : %.F90
	$(FC) -c $< $(FFLAGS)


all : opencl-multigrid

opencl-multigrid : opencl-multigrid.c
	$(CC) opencl-multigrid.c -o opencl-multigrid
	./opencl-multigrid

clean :
	rm -rf $(OBJS) *.vti *.dat *.mod *.o opencl-multigrid
