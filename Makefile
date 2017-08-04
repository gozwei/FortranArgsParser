FILES =  argsparser.f90 test.f90
OPTIONS =  
FTN_NOMPI = gfortran $(OPTIONS) 
FTN_MPI =  mpifort  -DPUREMPI $(OPTIONS)
FTN       = $(FTN_MPI)
OUTPUT = test.exe

.DEFAULT_GOAL := gnutest

GNU_FAST_OPTS = -cpp -O3 -ffast-math -march=native -g2 
GNU_STND_OPTS = -cpp -O2 -Wall -g2 
GNU_TEST_OPTS = -cpp -Wall -fbounds-check -pedantic-errors \
        -O0 -fbacktrace -ffpe-trap=zero,overflow -g -fcheck=all  

all: 
	$(FTN) $(GNU_FAST_OPTS) $(FILES) $(PNETCDF) -o $(OUTPUT) $(LIBS)
gnufast: 
	$(FTN) $(GNU_FAST_OPTS) $(FILES) $(PNETCDF) -o $(OUTPUT)
gnutest: 
	$(FTN) $(GNU_TEST_OPTS) $(FILES) $(PNETCDF) -o $(OUTPUT)
gnustand: 
	$(FTN) $(GNU_STND_OPTS) $(FILES) $(PNETCDF) -o $(OUTPUT)

clean:
	rm -r -f *.mod $(OUTPUT)