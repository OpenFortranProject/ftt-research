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
FC = mpif90

OPT_FLAG = -g

CFLAGS = $(OPT_FLAG) -DUNIX -DMAC
FFLAGS  = $(OPT_FLAG) -I $(FOR_OCL_DIR)
LDFLAGS = -DUNIX -DMAC -framework OpenCL

OBJS = forward_star.o
LIBS = -L $(FOR_OCL_DIR) -lforopencl

%.o : %.c
	$(CC) -c $< $(CFLAGS)

%.o : %.f90
	$(FC) -c $< $(FFLAGS)

%.o : %.F90
	$(FC) -c $< $(FFLAGS)


all : $(OBJS) dijkstra_cl

dijkstra_cl : $(OBJS) dijkstra_cl.o
	$(FC) -o $@ $(LDFLAGS) dijkstra_cl.o $(OBJS) $(LIBS)

check-gen : $(OFP_DIR)/LOPe/trans/lope-to-foropencl
	@sglri -p $(TBL) -i dijkstra_cafe.F90 | $(TO_FAST) | $(OFP_DIR)/LOPe/trans/lope-to-foropencl | $(TO_PP) | ast2text -p $(PP) > dijkstra_cl.F90

check : dijkstra_cl
	./dijkstra_cl

clean :
	rm -rf *.mod *.o dijkstra_cl
