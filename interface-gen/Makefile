CPP = g++
FC  = gfortran

OBJS = unparse_to_interface.o

EXES = test_igen

%.o : %.cpp
	$(CPP) -c $(CFLAGS) $<

%.o : %.f90
	$(FC) -c $(FFLAGS) $<

all : $(OBJS) $(EXES)

test_igen : test_igen.o $(OBJS)
	$(CPP) -o $@ $< $(OBJS)

check : all
	test_igen

clean :
	rm -f *.o $(OBJS) $(EXES)

# dependencies
#
unparse_to_interface.o : unparse_to_interface.cpp
