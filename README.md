# f90 derived type omp reduction

How best to do an OpenMP reduction on an array inside a derived type variable.
Methods enabled with #define at the top of `test_reduce.F90` were found to compile.

```
# (gcc 11.2, Intel 21.3.0, NVHPC 21.11)
$ export OMP_NUM_THREADS=8; for comp in GCC Intel NVHPC; do echo ${comp}; make clean; make COMPILER=${comp} && ./test_reduce; done
GCC
rm -rf obj
gfortran -c -o obj/test_reduce.o -O3 -fopenmp test_reduce.F90
gfortran -o test_reduce -O3 -fopenmp  obj/test_reduce.o
 serial:     2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 omp tmp:    2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 subr:       2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 subr_ser:   2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 assocfn:    2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 assocsf:    2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 doconcB:    2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
Intel
rm -rf obj
ifort -c -o obj/test_reduce.o -O3 -qopenmp test_reduce.F90
 serial:     2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 omp tmp:    2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 subr:       2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 subr_ser:   2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 assocfn:    2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 assocsf:    2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
 doconcB:    2500.00000       2501.00000       2501.00000       2501.00000       2501.00000       2501.00000       2500.00000       2500.00000
NVHPC
rm -rf obj
nvfortran -c -o obj/test_reduce.o -O3 -mp -stdpar=multicore test_reduce.F90
nvfortran -o test_reduce -O3 -mp -stdpar=multicore  obj/test_reduce.o
 serial:      2500.000        2501.000        2501.000        2501.000
    2501.000        2501.000        2500.000        2500.000
 omp tmp:     2500.000        2501.000        2501.000        2501.000
    2501.000        2501.000        2500.000        2500.000
 subr:                 NaN             NaN             NaN             NaN
             NaN             NaN             NaN             NaN
 subr_ser:    2500.000        2501.000        2501.000        2501.000
    2501.000        2501.000        2500.000        2500.000
 pointer:              NaN             NaN             NaN             NaN
             NaN             NaN             NaN             NaN
 ptr ser:     2500.000        2501.000        2501.000        2501.000
    2501.000        2501.000        2500.000        2500.000
 ptr fnc:              NaN             NaN             NaN             NaN
             NaN             NaN             NaN             NaN
 assoc:                NaN             NaN             NaN             NaN
             NaN             NaN             NaN             NaN
 assocfn:              NaN             NaN             NaN             NaN
             NaN             NaN             NaN             NaN
 assocsf:     2500.000        2501.000        2501.000        2501.000
    2501.000        2501.000        2500.000        2500.000
 doconcB:     866.0000        872.0000        884.0000        879.0000
    805.0000        872.0000        868.0000        871.0000
 doconcL:     598.0000        602.0000        603.0000        611.0000
    601.0000        602.0000        437.0000        603.0000
 doconcTR:    2500.000        2501.000        2501.000        2501.000
    2501.000        2501.000        2500.000        2500.000
```
