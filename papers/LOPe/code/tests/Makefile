CLC = /System/Library/Frameworks/OpenCL.framework/Versions/Current/Libraries/openclc

FOR_OCL_DIR = ../../../../opencl/ForOpenCL/

CFLAGS = 
LDFLAGS = -Wl,-no_pie -L$(FOR_OCL_DIR) -lforopencl -framework OpenCL

## Fortran targets
#  ---------------

FC = ifort
FCFLAGS = -g -I $(FOR_OCL_DIR)

%.o: %.f90
	@echo 'Building for OS $(OSUPPER) file: $<'
	$(FC) -c $(FCFLAGS) -o $@ $<


all : elemental_add_1d

elemental_add_1d : elemental_add_1d.f90
	$(FC) elemental_add_1d.f90 -o elemental_add_1d $(FCFLAGS) $(LDFLAGS)

check : elemental_add_1d
	@elemental_add_1d

clean :
	rm -f elemental_add_1d
	rm -f *.o *.mod
