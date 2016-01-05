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

LIB_OBJS = Timer.o OpenCLTypes.o CLBuffer.o CLKernel.o CLDevice.o \
           opencl_utils.o opencl_test.o ForOpenCL.o

OBJS += OpenCLInterfaces.o $(LIB_OBJS)

LIBS += $(OBJS)

APPS = multigrid elementals 

%.o: %.f90
	@echo 'Building for OS $(OSUPPER) file: $<'
	$(FC) -c $(FCFLAGS) -o $@ $<

all: $(OBJS) libforopencl.a $(APPS)

libforopencl.a: $(OBJS)
	ar -r libforopencl.a $(LIB_OBJS)
	ranlib libforopencl.a

elementals: elementals.o $(OBJS)
	$(LINK) $(FCFLAGS) -o $@ $< $(LIBS)

multigrid: multigrid.o $(OBJS)
	$(LINK) $(FCFLAGS) -o $@ $< $(LIBS)

opencl_utils.o: opencl_utils.c
	$(CC) -c $(CFLAGS) $<

opencl_test.o: opencl_test.c
	$(CC) -c $(CFLAGS) $<

opencl_test: opencl_test.o # $(OBJS)
	$(CC) -o $@ $<


# dependencies
#

OpenCLInterfaces.o: Timer.o OpenCLTypes.o

CLBuffer.o: OpenCLTypes.o OpenCLInterfaces.o

elemental_add.o: elemental_add.f90 $(OBJS)

multigrid.o: multigrid.f90 $(OBJS)

clean:
	rm -f $(OBJS)
	rm -f *.mod *.o
	rm -f $(APPS)
	rm -f libforopencl.a
