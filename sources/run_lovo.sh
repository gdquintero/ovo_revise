rm -f lovo

gfortran -c -O3 -Wall sort.f90
gfortran -c -O3 -Wall lovo.f90
gfortran -L$PWD sort.o lovo.o -o lovo

./lovo