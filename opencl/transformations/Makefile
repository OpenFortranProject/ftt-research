# Example Makefile

# Location of ROSE directory
ROSE_DIR ?= $(HOME)/ftt/rose-trunk/compileTree
ROSE_BUILD_DIR ?= $(HOME)/ftt/rose-trunk/compileTree

# Location of ROSE include directory 
ROSE_INCLUDE_DIR := $(ROSE_DIR)/include

# Location of ROSE library directory
ROSE_LIB_DIR := $(ROSE_DIR)/lib
ROSE_LIBS := $(ROSE_LIB_DIR)/librose.la

# Location of ROSE libtool
ROSE_LIBTOOL := $(ROSE_BUILD_DIR)/libtool

BOOST_INCLUDE_PATH ?= /usr

OBJS := FortranAnalysis.lo FortranTraversal.lo Util.lo

CXXFLAGS := -g -Wall -Wextra -Werror -pedantic -pedantic-errors -isystem $(ROSE_INCLUDE_DIR) -isystem $(BOOST_INCLUDE_PATH)

# Default make rule to use
all: build_opencl

build_opencl: build_opencl.lo FortranAnalysis.lo FortranTraversal.lo Util.lo $(OBJS)
	$(ROSE_LIBTOOL) --mode=link g++ $(CXXFLAGS) -o $@ $< $(OBJS) $(ROSE_LIBS)

build_opencl.lo: build_opencl.cpp FortranAnalysis.hpp FortranTraversal.hpp Util.hpp
	$(ROSE_LIBTOOL) --mode=compile g++ $(CXXFLAGS) -I$(ROSE_INCLUDE_DIR) -c -o $@ $<

FortranAnalysis.lo: FortranAnalysis.cpp FortranAnalysis.hpp Util.hpp
	$(ROSE_LIBTOOL) --mode=compile g++ $(CXXFLAGS) -I$(ROSE_INCLUDE_DIR) -c -o $@ $<

FortranTraversal.lo: FortranTraversal.cpp FortranTraversal.hpp Util.hpp
	$(ROSE_LIBTOOL) --mode=compile g++ $(CXXFLAGS) -I$(ROSE_INCLUDE_DIR) -c -o $@ $<

Util.lo: Util.cpp Util.hpp
	$(ROSE_LIBTOOL) --mode=compile g++ $(CXXFLAGS) -I$(ROSE_INCLUDE_DIR) -c -o $@ $<

clean:
	rm -f build_opencl build_opencl.lo $(OBJS)

