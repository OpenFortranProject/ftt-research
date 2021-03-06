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

COMPASS_DIR := compass

CHECKER_NAME := testChecker

USER_CODE_FLAGS := -Wall -Weffc++ -Werror -Wextra -pedantic -pedantic-errors -ggdb3
# TODO: Disable this once the code is ready
USER_CODE_FLAGS += -Wno-unused-variable -Wno-unused-parameter
CXX_FLAGS := -fPIC -Wno-deprecated -isystem $(ROSE_INSTALL)/include $(ROSE_OPTIONAL_LIBS_INCLUDE) -isystem $(BOOST_INCLUDE_PATH) -D_DEBUG -isystem $(COMPASS_DIR)

LINKER_FLAGS := $(ROSE_LIB) $(ROSE_OPTIONAL_LIBS) $(LOCAL_COMPASS_LIB) $(PATH_TO_SPECIFIC_LIBSTDCXX)

all: arrayIndex

libcompass.so: $(addprefix $(COMPASS_DIR)/,compass.C compass.h)
	g++ -shared -o libcompass.so $< $(CXX_FLAGS)

PathGrader.o: PathGrader.C PathGrader.h
	g++ $(USER_CODE_FLAGS) $(CXX_FLAGS) $< -c -o $@

arrayIndex: arrayIndex.C arrayIndex.h arrayIndexMain.C libcompass.so PathGrader.o
	g++ $(USER_CODE_FLAGS) $(CXX_FLAGS) -o arrayIndex  arrayIndexMain.C arrayIndex.C PathGrader.o $(LINKER_FLAGS)

test: arrayIndex arrayIndexTest1.C
	./arrayIndex arrayIndexTest1.C

clean:
	-rm -f libcompass.so "arrayIndex" PathGrader.o

