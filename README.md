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
serial:    2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
omp tmp:   2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
subr:      2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
subr ser:  2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
ass fn:    2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
ass s fn:  2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
doc bl:    2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
Intel
rm -rf obj
ifort -c -o obj/test_reduce.o -O3 -qopenmp test_reduce.F90
serial:    2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
omp tmp:   2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
subr:      2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
subr ser:  2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
ass fn:    2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
ass s fn:  2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
doc bl:    2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
NVHPC
rm -rf obj
nvfortran -c -o obj/test_reduce.o -O3 -mp -stdpar=multicore test_reduce.F90
nvfortran -o test_reduce -O3 -mp -stdpar=multicore  obj/test_reduce.o
serial:    2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
omp tmp:   2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
subr:        NaN     NaN     NaN     NaN     NaN     NaN     NaN     NaN
subr ser:  2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
pointer:     NaN     NaN     NaN     NaN     NaN     NaN     NaN     NaN
ptr ser:   2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
ptr fn:      NaN     NaN     NaN     NaN     NaN     NaN     NaN     NaN
ass:         NaN     NaN     NaN     NaN     NaN     NaN     NaN     NaN
ass fn:      NaN     NaN     NaN     NaN     NaN     NaN     NaN     NaN
ass s fn:  2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
doc bl:     854.    861.    833.    862.    866.    855.    810.    861.
doc loc:    201.    647.    652.    644.    641.    639.    640.    641.
doc t r:   2500.   2501.   2501.   2501.   2501.   2501.   2500.   2500.
```

## NVHPC bug
```
$ export OMP_NUM_THREADS=1; make clean && make test_reduce_mwe COMPILER=NVHPC && ./test_reduce_mwe
nvfortran -c -o obj/test_reduce_mwe.o -O3 -mp -stdpar=multicore -Mbounds test_reduce_mwe.F90
nvfortran -o test_reduce_mwe -O3 -mp -stdpar=multicore -Mbounds  obj/test_reduce_mwe.o
0: Subscript out of range for array tmp_arr (test_reduce_mwe.F90: 46)
    subscript=1, lower bound=140727224399784, upper bound=140727224407104, dimension=1

$ nvfortran -V

nvfortran 21.11-0 64-bit target on x86-64 Linux -tp haswell 
NVIDIA Compilers and Tools
Copyright (c) 2021, NVIDIA CORPORATION & AFFILIATES.  All rights reserved.

```
