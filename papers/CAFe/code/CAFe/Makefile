OFP_DIR = /Users/rasmus/ofp-sdf
FOR_OCL_DIR = /Users/rasmus/ofp-research/opencl/ForOpenCL

include $(OFP_DIR)/make.inc

SYNTAX_DIR = $(OFP_DIR)/LOPe/syntax
TRANS_DIR  = $(OFP_DIR)/fortran/trans
PP_DIR     = $(OFP_DIR)/fortran/pp

TBL     = $(SYNTAX_DIR)/LOPe.tbl
TO_SIM  = $(TRANS_DIR)/ofp-simplify
TO_FAST = $(TRANS_DIR)/ofp2fast
TO_PP   = $(TRANS_DIR)/fast2pp
PP      = $(PP_DIR)/Fortran.pp

CLC = /System/Library/Frameworks/OpenCL.framework/Versions/Current/Libraries/openclc


CC = cc
FC = gfortran

OPT_FLAG = -g

CFLAGS = $(OPT_FLAG) $(ST_CFLAGS) -I $(OFP_DIR)/LOPe/syntax -I $(OFP_DIR)/fortran/syntax -I $(OFP_DIR)/fortran/trans
FFLAGS  = $(OPT_FLAG)
LDFLAGS = $(ST_LDFLAGS) -la stratego-lib

OBJS = io.o error.o # jacobi.o vtk.o vtk_output.o
LIBS = 

%.o : %.c
	$(CC) -c $< $(CFLAGS)

%.o : %.f90
	$(FC) -c $< $(FFLAGS)

%.o : %.F90
	$(FC) -c $< $(FFLAGS)


all : $(OBJS) poisson-multigrid_1d_cl

poisson-multigrid_1d_cl : $(OBJS) poisson-multigrid_1d_cl.o
	$(FC) -o $@ $(LDFLAGS) poisson-multigrid_1d_cl.o $(OBJS) $(LIBS)

poisson-multigrid : $(OBJS) poisson-multigrid.o
	$(FC) -o $@ $(LDFLAGS) poisson-multigrid.o $(OBJS) $(LIBS)

check : $(OFP_DIR)/LOPe/trans/lope-to-foropencl
	@sglri -p $(TBL) -i poisson-multigrid_1d.f90 | $(TO_FAST) | $(OFP_DIR)/LOPe/trans/lope-to-foropencl | $(TO_PP) | ast2text -p $(PP) > poisson-multigrid_1d_cl.f90

check-run : poisson-multigrid
	rm -f *.vti *.dat
	./poisson-multigrid

clean :
	rm -rf $(OBJS) *.vti *.dat *.mod *.o poisson-multigrid_1d_cl
