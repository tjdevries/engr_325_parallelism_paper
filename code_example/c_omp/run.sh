#!/bin/bash

make

echo "OpenMP Results:" > result.txt

export OMP_NUM_THREADS=1
./parallel_sum $1 >> result.txt
export OMP_NUM_THREADS=2
./parallel_sum $1 >> result.txt

cat result.txt
