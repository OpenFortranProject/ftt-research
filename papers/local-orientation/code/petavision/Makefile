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

FOCL_DIR = ../../../../opencl/f90

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

FINCLUDES = -I$(FOCL_DIR)

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
CFLAGS   += $(INCLUDES)  $(COMMONFLAGS)
FCFLAGS  += $(FINCLUDES) $(COMMONFLAGS)

OBJS = weight_params.o weight_type.o

LIBS += $(OBJS) -L$(FOCL_DIR) -lforopencl

APPS = recv_synaptic_input test_atomics

%.o: %.f90
	@echo 'Building for OS $(OSUPPER) file: $<'
	$(FC) -c $(FCFLAGS) -o $@ $<

all: $(OBJS) $(APPS)

recv_synaptic_input: recv_synaptic_input.o $(OBJS)
	$(LINK) $(FCFLAGS) -o $@ $< $(LIBS)

test_atomics: test_atomics.o $(OBJS)
	$(LINK) $(FCFLAGS) -o $@ $< $(LIBS)

weight_params.o: weight_params.f90
	@echo 'Building for OS $(OSUPPER) file: $<'
	$(FC) -c $(FCFLAGS) -o $@ $<

# dependencies
#

test_synaptic_input.o: weight_params.o weight_type.f90
test_atomics.o: test_atomics.f90
weight_type.o: weight_params.o weight_type.f90

clean:
	rm -f *.mod *.o
	rm -f $(APPS)
