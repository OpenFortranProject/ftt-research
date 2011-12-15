# Example Makefile

# Location of ROSE directory
ROSE_DIR ?= $(HOME)/ROSE/install-tree-12749
ROSE_BUILD_DIR ?= $(HOME)/ROSE/build-tree-12749

# Location of ROSE include directory 
ROSE_INCLUDE_DIR = $(ROSE_DIR)/include 

# Location of ROSE library directory
ROSE_LIB_DIR = $(ROSE_DIR)/lib
ROSE_LIBS = $(ROSE_LIB_DIR)/librose.la

# Location of ROSE libtool
ROSE_LIBTOOL = $(ROSE_BUILD_DIR)/libtool

OBJS = FortranAnalysis.lo FortranTraversal.lo

# Default make rule to use
all: build_opencl

build_opencl: build_opencl.lo $(OBJS)
	$(ROSE_LIBTOOL) --mode=link g++ -o $@ $< $(OBJS) $(ROSE_LIBS)

build_opencl.lo: build_opencl.cpp
	$(ROSE_LIBTOOL) --mode=compile g++ -I$(ROSE_INCLUDE_DIR) -c -o $@ $<

FortranAnalysis.lo: FortranAnalysis.cpp FortranAnalysis.hpp
	$(ROSE_LIBTOOL) --mode=compile g++ -I$(ROSE_INCLUDE_DIR) -c -o $@ $<

FortranTraversal.lo: FortranTraversal.cpp FortranTraversal.hpp
	$(ROSE_LIBTOOL) --mode=compile g++ -I$(ROSE_INCLUDE_DIR) -c -o $@ $<

clean:
	rm -f build_opencl build_opencl.lo $(OBJS)

