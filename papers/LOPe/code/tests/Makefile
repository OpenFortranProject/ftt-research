CLC = /System/Library/Frameworks/OpenCL.framework/Versions/Current/Libraries/openclc

FOR_OCL_DIR = ../../../../opencl/ForOpenCL

FC = mpifort

FCFLAGS = -g -I$(FOR_OCL_DIR) -I../../../../parallel-halos
LDFLAGS = -Wl,-no_pie -L$(FOR_OCL_DIR) -lforopencl -framework OpenCL -L../../../../parallel-halos -lhalos

## Fortran targets
#  ---------------

%.o: %.f90
	@echo 'Building for OS $(OSUPPER) file: $<'
	$(FC) -c $(FCFLAGS) -o $@ $<


all : elemental_add_1d elemental_add_2d elemental_add_3d

elemental_add_1d : elemental_add_1d.f90
	$(FC) elemental_add_1d.f90 -o elemental_add_1d $(FCFLAGS) $(LDFLAGS)

elemental_add_2d : elemental_add_2d.f90
	$(FC) elemental_add_2d.f90 -o elemental_add_2d $(FCFLAGS) $(LDFLAGS)

elemental_add_3d : elemental_add_3d.f90
	$(FC) elemental_add_3d.f90 -o elemental_add_3d $(FCFLAGS) $(LDFLAGS)

check : elemental_add_2d
	@elemental_add_2d

clean :
	rm -f elemental_add_1d elemental_add_2d elemental_add_3d
	rm -f *.o *.mod *.dwarf
