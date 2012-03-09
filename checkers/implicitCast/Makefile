# ************************************************************************************
# *******  Users SHOULD modify the makefile variables at the top of this file ********
# ************************************************************************************

# Set variable ROSE_INSTALL to your ROSE_INSTALL installation like this.
ROSE_INSTALL ?= $(HOME)/ftt/rose-trunk/compileTree

# If ROSE is configured with Dwarf support then we need libdwarf.a
# ROSE_OPTIONAL_LIBS = /home/fangling/ROSE/DWARF/dwarf-20081013/libdwarf/libdwarf.a -lelf

# A version of libcompass.so will be built locally to allow the checker to run
LOCAL_COMPASS_LIB := -L`pwd` -Wl,-rpath `pwd` -lcompass 

# Link syntax required for ROSE (using instaled version of ROSE)
ROSE_LIB := -L$(ROSE_INSTALL)/lib -Wl,-rpath $(ROSE_INSTALL)/lib -lrose 

# Specify specific path to find libstdc++.so (required when not using the default system compiler)
# This is required at least on the LLNL computers using non-default versions of GNU compilers.
# PATH_TO_SPECIFIC_LIBSTDCXX = -Wl,--rpath -Wl,/usr/apps/gcc/4.2.2-64bit/lib64

# Boost is required in ROSE, so we need the path.
BOOST_INCLUDE_PATH ?= /usr

# If ROSE is configured with optional libs then we need the associated include paths.
# ROSE_OPTIONAL_LIBS_INCLUDE = -I/home/fangling/ROSE/DWARF/dwarf-20081013/libdwarf

# ************************************************************************************
# **********  Users should NOT have to modify the makefile below this point **********
# ************************************************************************************

CHECKER_NAME := testChecker

CXX_FLAGS := -fPIC -Wall -Wextra -Werror -pedantic -pedantic-errors -isystem $(ROSE_INSTALL)/include $(ROSE_OPTIONAL_LIBS_INCLUDE) -isystem $(BOOST_INCLUDE_PATH)

LINKER_FLAGS := $(ROSE_LIB) $(ROSE_OPTIONAL_LIBS) $(LOCAL_COMPASS_LIB) $(PATH_TO_SPECIFIC_LIBSTDCXX)

all: implicitCast

libcompass.so: compass.h compass.C
	g++ -shared -o libcompass.so compass.C $(CXX_FLAGS)

TypeDescription.o: TypeDescription.cpp TypeDescription.hpp
	g++ $(CXX_FLAGS) -c $< -o $@

ConfigParser.o: ConfigParser.cpp ConfigParser.hpp
	g++ $(CXX_FLAGS) -c $< -o $@

implicitCast: implicitCast.C implicitCastMain.C libcompass.so ConfigParser.o TypeDescription.o
	g++ -o implicitCast implicitCastMain.C implicitCast.C $(CXX_FLAGS) $(LINKER_FLAGS) ConfigParser.o TypeDescription.o

test: implicitCast implicitCastTest1.f90
	./implicitCast implicitCastTest1.f90

clean:
	-rm -f libcompass.so "implicitCast" ConfigParser.o

