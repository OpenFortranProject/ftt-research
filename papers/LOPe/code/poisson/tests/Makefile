FCC = ifort
FFLAGS = -I..

%.o : %.f90
	$(FCC) -c $(FFLAGS) -o $@ $<

all : test_restrict test_prolongate

test_restrict : test_restrict.o ../restriction_lope.o
	$(FCC) -o test_restrict test_restrict.o ../restriction_lope.o

test_prolongate : test_prolongate.o ../prolongation_lope.o
	$(FCC) -o test_prolongate test_prolongate.o ../prolongation_lope.o

../restriction_lope.o : ../restriction_lope.f90
../prolongation_lope.o : ../prolongation_lope.f90

check : test_restrict test_prolongate
	@echo "Running test_restrict..."
	@test_restrict
	@echo "Running test_prolongate..."
	@test_prolongate

clean :
	rm -f *.o *.mod test_restrict test_prolongate

