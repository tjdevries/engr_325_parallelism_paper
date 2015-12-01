/* parallel_sum.c sums the numbers from 1 to N.
 * TJ DeVries, for ENGR 325 at Calvin College.
 */

#include <stdio.h>      // printf(), etc.
#include <stdlib.h>     // exit()
#include <math.h>       // sqrt()
#include <mpi/mpi.h>

/* retrieve desired maximum from commandline arguments
 * parameters: argc: the argument count
 *             argv: the argument vector
 * return: the number of trapezoids to be used.
 */
unsigned long long processCommandLine(int argc, char** argv) {
   if (argc == 1) { return 10; }
   else if (argc == 2) { return strtoull( argv[1], 0, 10 ); } 
   else {
       fprintf(stderr, "Usage: ./parallel_sum [maximum]");
       exit(1);
   }
}

unsigned long long sum_range(unsigned long long min, unsigned long long max) {
    unsigned long long i, sum;
    sum = 0;

    for(i = min; i < max; i++) {
        sum += i;
    }

    return sum;
}

int main(int argc, char** argv) {
    unsigned long long localSum = 0;
    unsigned long long finalSum;

    // Timing values
    double startTime, stopTime;

    int numProc = -1;
    int id = -1;

    // Start our MPI items
    MPI_Init(&argc, &argv);                  //  Initialize MPI from command line arguments
    MPI_Comm_size(MPI_COMM_WORLD, &numProc); //  Find the number of processes
    MPI_Comm_rank(MPI_COMM_WORLD, &id);      //  Find out which process "we" are

    // Start the timer
    startTime = MPI_Wtime();

    // Get the maximum N
    unsigned long long maximum = processCommandLine(argc, argv);

    //  We will assign each PE it's own section of that section
    long double width = maximum / numProc;

    long double start = id * width;
    long double end = (id + 1) * width;

    localSum = sum_range(id * width, (id + 1) * width);

    MPI_Reduce(&localSum, &finalSum, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, 0, MPI_COMM_WORLD);

    stopTime = MPI_Wtime();

    if (id == 0) {
        printf("NumProc: %d, Time: %7f, Sum: %llu\n", 
                numProc, stopTime - startTime, finalSum);
    }

    MPI_Finalize();

    return 0;
}

