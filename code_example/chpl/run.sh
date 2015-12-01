#!/bin/bash

make

echo "Chapel Results:" > result.txt
./sequential_sum --maximum=$1 >> result.txt
./parallel_sum --maximum=$1 --numProc=2 >> result.txt

cat result.txt
