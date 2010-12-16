#
# compile
#
#g++ -W -Wall -Wimplicit -Wswitch -Wformat -Wchar-subscripts -Wparentheses -Wmultichar -Wtrigraphs -Wpointer-arith -Wcast-align -Wreturn-type -Wno-unused-function  -m64 -fno-strict-aliasing -I. -I../../..//OpenCL//common//inc -I../../..//shared//inc -DUNIX -O3  -o obj/release/oclVectorAdd.cpp.o -c oclVectorAdd.cpp

#
# link
#
#g++ -fPIC -m64 -o ../../..//OpenCL//bin//linux/release/oclVectorAdd obj/release/oclVectorAdd.cpp.o  -L/usr/lib64/ -L../../..//OpenCL//common//lib -L../../..//shared/lib/ -L../../..//shared//lib/linux  -lOpenCL  -L/usr/lib64/ -L../../..//OpenCL//common//lib -L../../..//shared/lib/ -L../../..//shared//lib/linux   -loclUtil_x86_64 -lshrutil_x86_64

# debugging flag
dbg := 0

# fortran compiler flag
intel_fortran := 0

# detect OS                                                                               
OSUPPER = $(shell uname -s 2>/dev/null | tr [:lower:] [:upper:])
OSLOWER = $(shell uname -s 2>/dev/null | tr [:upper:] [:lower:])
# 'linux' is output for Linux system, 'darwin' for OS X                                   
DARWIN = $(strip $(findstring DARWIN, $(OSUPPER)))

# detect architecture
OSARCH= $(shell uname -m)

INCDIR  ?= .

ifeq ($(DARWIN),)
   SDKROOTDIR   := /home/rasmussn/local/NVIDIA_GPU_Computing_SDK
   LIBDIR       := $(SDKROOTDIR)/shared/lib/
   SHAREDDIR    := $(SDKROOTDIR)/shared/
   SHAREDLIBDIR := $(SDKROOTDIR)/shared/lib/linux
   OCLROOTDIR   := $(SDKROOTDIR)/OpenCL/
   OCLCOMMONDIR := $(OCLROOTDIR)/common/
   OCLLIBDIR    := $(OCLCOMMONDIR)/lib
endif


# Compilers
ifeq ($(intel_fortran),1)
   FC    := ifort
else
   FC     := gfortran
endif
CC     := gcc
LINK   := $(FC) -fPIC

# Includes
ifeq ($(DARWIN),)
   INCLUDES += -I$(INCDIR) -I$(OCLCOMMONDIR)/inc -I$(SHAREDDIR)/inc
else
   INCLUDES += -I$(INCDIR)
endif

# Warning flags
CWARN_FLAGS := -W -Wall

# architecture flag for nvcc and gcc compilers build
LIB_ARCH := $(OSARCH)

ifeq ($(DARWIN),)
   ARCH_FLAGS += -m64
else
   ARCH_FLAGS += -m64
endif

# Compiler-specific flags
CFLAGS   := $(CWARN_FLAGS) $(ARCH_FLAGS)
FCFLAGS  := $(ARCH_FLAGS)
LINK     += $(ARCH_FLAGS)

# Includes
ifneq ($(DARWIN),)
else
endif

# Common flags
COMMONFLAGS += -DUNIX

# Add Mac Flags
ifneq ($(DARWIN),)
   COMMONFLAGS += -DMAC
endif

# Debug/release configuration
ifeq ($(dbg),1)
   COMMONFLAGS += -g
else
   COMMONFLAGS += -O3
   CFLAGS      += -fno-strict-aliasing
endif

# Libs
ifneq ($(DARWIN),)
   LIBS  := -framework OpenCL
else
   LIBS := -L${OCLLIBDIR} -L$(LIBDIR) -L$(SHAREDDIR)/lib/$(OSLOWER) 
   LIBS += -lOpenCL
endif

# Add common flags
CFLAGS   += $(INCLUDES) $(COMMONFLAGS)
FCFLAGS  += $(COMMONFLAGS)

OBJS = Timer.o OpenCLTypes.o OpenCLInterfaces.o CLBuffer.o CLKernel.o CLDevice.o \
       OpenCL.o opencl_utils.o opencl_test.o test_shift_impl.o

LIBS += $(OBJS)

APPS = test_shift update_weights bandwidth transpose shallow_water shallow_water_f

%.o: %.f90
	@echo 'Building for OS $(OSUPPER) file: $<'
	$(FC) -c $(FCFLAGS) -o $@ $<

all: $(OBJS) $(APPS)

shallow_water: shallow_water.o $(OBJS)
	$(LINK) $(FCFLAGS) -o $@ $< $(LIBS)

shallow_water_f: shallow_water_f.o $(OBJS)
	$(LINK) $(FCFLAGS) -o $@ $< $(LIBS)

test_shift: test_shift.o $(OBJS)
	$(LINK) $(FCFLAGS) -o $@ $< $(LIBS)

update_weights: update_weights.o $(OBJS)
	$(LINK) -o $@ $< $(LIBS)

bandwidth: bandwidth.o $(OBJS)
	$(LINK) -o $@ $< $(LIBS)

transpose: transpose.o $(OBJS)
	$(LINK) -o $@ $< $(LIBS)

opencl_utils.o: ../c/opencl_utils.c
	$(CC) -c $(CFLAGS) $<

opencl_test.o: ../c/opencl_test.c
	$(CC) -c $(CFLAGS) $<

# dependencies
#

test_shift.o: test_shift.f90 $(OBJS)

test_shift_impl.o: test_shift_impl.f90

update_weights.o: update_weights.f90 $(OBJS)

bandwidth.o: bandwidth.f90 $(OBJS)

transpose.o: transpose.f90 $(OBJS)

OpenCL.o: OpenCL.f90 CLBuffer.o CLDevice.o OpenCLTypes.o OpenCLInterfaces.o

CLDevice.o: CLDevice.f90 OpenCLTypes.o OpenCLInterfaces.o CLBuffer.o 

CLKernel.o: CLKernel.f90 OpenCLTypes.o OpenCLInterfaces.o

CLBuffer.o: CLKernel.f90 OpenCLTypes.o OpenCLInterfaces.o

Timer.o : Timer.f90

OpenCLInterfaces.o: OpenCLTypes.o

clean:
	rm -f $(OBJS) test_shift.o update_weights.o bandwidth.o
	rm -f *.mod
	rm -f $(APPS)
