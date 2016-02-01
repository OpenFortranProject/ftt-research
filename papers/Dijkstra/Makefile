FC = mpif90
FFLAGS = -g

OBJS = dijkstra.o forward_star.o

all: dijkstra_serial

dijkstra_serial: dijkstra_serial.F90 $(OBJS)
	$(FC) -o dijkstra_serial $(FFLAGS) dijkstra_serial.F90 $(OBJS)

dijkstra.o: dijkstra.F90
	$(FC) -c $(FFLAGS) dijkstra.F90

forward_star.o: forward_star.F90
	$(FC) -c $(FFLAGS) forward_star.F90

check: dijkstra_serial
	@dijkstra_serial

clean:
	rm -f *.o *.mod dijkstra_serial
