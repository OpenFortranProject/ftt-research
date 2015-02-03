#
# This makefile should work on either Linux or Mac OSX (Darwin).
#

#
# Provide the location of the NVIDIA SDK
#
#             /usr/local/NVIDIA_GPU_Computing_SDK
#
SDKROOTDIR := /usr/local/packages/cuda/sdk-4.2


# debugging flag
dbg := 0

# fortran compiler flag
intel_fortran := 1

# detect OS
OSUPPER = $(shell uname -s 2>/dev/null | tr [:lower:] [:upper:])
OSLOWER = $(shell uname -s 2>/dev/null | tr [:upper:] [:lower:])
# 'linux' is output for Linux system, 'darwin' for OS X
DARWIN = $(strip $(findstring DARWIN, $(OSUPPER)))

# detect architecture
OSARCH= $(shell uname -m)

INCDIR  ?= .

ifeq ($(DARWIN),)
   LIBDIR       := $(SDKROOTDIR)/shared/lib/
   SHAREDDIR    := $(SDKROOTDIR)/shared/
   SHAREDLIBDIR := $(SDKROOTDIR)/shared/lib/linux
   OCLROOTDIR   := $(SDKROOTDIR)/OpenCL/
   OCLCOMMONDIR := $(OCLROOTDIR)/common/
   OCLLIBDIR    := $(OCLCOMMONDIR)/lib
endif

# Compilers
ifeq ($(intel_fortran),1)
   FC    := mpifort
else
   FC    := mpifort
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
   ARCH_FLAGS +=
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

#--------------------------------------------------

FOR_OCL_DIR = ../../../../opencl/ForOpenCL

FCFLAGS += -I$(FOR_OCL_DIR) -I../../../../parallel-halos

LIBS += -L$(FOR_OCL_DIR) -lforopencl -L../../../../parallel-halos -lhalos

#
# May not work with Intel compiler on Linux
#
#LDFLAGS = -Wl,-no_pie

## Fortran targets
#  ---------------

%.o: %.f90
	@echo 'Building for OS $(OSUPPER) file: $<'
	$(FC) -c $(FCFLAGS) -o $@ $<


all : elemental_add_1d elemental_add_2d elemental_add_3d

elemental_add_1d : elemental_add_1d.f90
	$(LINK) elemental_add_1d.f90 -o elemental_add_1d $(FCFLAGS) $(LDFLAGS) $(LIBS)

elemental_add_2d : elemental_add_2d.f90
	$(LINK) elemental_add_2d.f90 -o elemental_add_2d $(FCFLAGS) $(LDFLAGS) $(LIBS)

elemental_add_3d : elemental_add_3d.f90
	$(LINK) elemental_add_3d.f90 -o elemental_add_3d $(FCFLAGS) $(LDFLAGS) $(LIBS)

elemental_add_3d_perf : elemental_add_3d_perf.f90
	$(LINK) elemental_add_3d_perf.f90 -o elemental_add_3d_perf $(FCFLAGS) $(LDFLAGS) $(LIBS)

check : elemental_add_3d_perf
	@mpirun -np 2 elemental_add_3d_perf

clean :
	rm -f elemental_add_1d elemental_add_2d elemental_add_3d elemental_add_3d_perf
	rm -f *.o *.mod *.dwarf
