MLBUILDER=mlton

all: clean driver

driver: sources.cm sources.mlb
ifeq (${MLBUILDER}, ml-build)
	ml-build sources.cm Driver.main driver
	heap2exec driver.x86-darwin driver
else ifeq (${MLBUILDER}, mlton)
	mlton -output driver $(MLTONARGS) sources.mlb
else
	echo "Don't know how to build with $(MLBUILDER)" && exit 1
endif

clean:
	rm -f *~
	rm -Rf .cm
	rm -f driver driver.x86-linux driver.x86-darwin
