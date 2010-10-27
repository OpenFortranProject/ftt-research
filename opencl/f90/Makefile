FC = ifort
CC = gcc

FCFLAGS = -g
CFLAGS = -g -arch i386

OBJS = OpenCLTypes.o OpenCLInterfaces.o CLBuffer.o CLKernel.o CLDevice.o \
       OpenCL.o opencl_utils.o opencl_test.o

LIBS = -framework OpenCL $(OBJS)

%.o: %.f90
	@echo 'Building file: $<'
	$(FC) -c $(FCFLAGS) -o $@ $<

all: forcl

forcl: forcl.o $(OBJS)
	$(FC) -o $@ $< $(LIBS)

forcl.o: forcl.f90 $(OBJS)
	$(FC) -c $<

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

OpenCLInterfaces.o: OpenCLTypes.o

clean:
	rm -f $(OBJS) forcl.o
	rm -f *.mod
	rm -f forcl
