# Compile the parallel and sequential portions of our project
parallel_CC=mpicc
sequential_cc=gcc
CFLAGS=

all: parallel_sum sequential_sum

parallel_sum: parallel_sum.c
	$(parallel_CC) parallel_sum.c -o parallel_sum

sequential_sum: sequential_sum.c
	$(sequential_cc) sequential_sum.c -o sequential_sum
