#!/bin/bash

make
echo "C with MPI Results: " > result.txt

./sequential_sum $1 >> result.txt
mpirun -np 2 ./parallel_sum $1 >> result.txt
