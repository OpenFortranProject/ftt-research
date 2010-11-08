#FC = ifort
FC = gfortran
CC = gcc

FCFLAGS = -g -m64
CFLAGS = -g -arch i386
CFLAGS = -g

OBJS = OpenCLTypes.o OpenCLInterfaces.o CLBuffer.o CLKernel.o CLDevice.o \
       OpenCL.o opencl_utils.o opencl_test.o Timer.o

LIBS = -framework OpenCL $(OBJS)

APPS = forcl update_weights

%.o: %.f90
	@echo 'Building file: $<'
	$(FC) -c $(FCFLAGS) -o $@ $<

all: $(OBJS) $(APPS)

forcl: forcl.o $(OBJS)
	$(FC) $(FCFLAGS) -o $@ $< $(LIBS)

update_weights: update_weights.o $(OBJS)
	$(FC) -o $@ $< $(LIBS)

forcl.o: forcl.f90 $(OBJS)
	$(FC) -c $(FCFLAGS) $<

opencl_utils.o: ../c/opencl_utils.c
	$(CC) -c $(CFLAGS) $<

opencl_test.o: ../c/opencl_test.c
	$(CC) -c $(CFLAGS) $<

# dependencies
#

OpenCL.o: OpenCL.f90 CLBuffer.o CLDevice.o OpenCLTypes.o OpenCLInterfaces.o

CLDevice.o: CLDevice.f90 OpenCLTypes.o OpenCLInterfaces.o CLBuffer.o 

CLKernel.o: CLKernel.f90 OpenCLTypes.o OpenCLInterfaces.o

CLBuffer.o: CLKernel.f90 OpenCLTypes.o OpenCLInterfaces.o

Timer.o : Timer.f90

OpenCLInterfaces.o: OpenCLTypes.o

clean:
	rm -f $(OBJS) forcl.o update_weights.o
	rm -f *.mod
	rm -f $(APPS)
