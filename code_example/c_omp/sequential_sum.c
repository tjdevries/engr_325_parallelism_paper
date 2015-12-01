/* sequential_sum.c sums the numbers from 1 to N.
 * TJ DeVries, for ENGR 325 at Calvin College.
 */

#include <stdio.h>      // printf(), etc.
#include <stdlib.h>     // exit()
#include <math.h>       // sqrt()
#include <time.h>		// Timer

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
    sum = 0;                     //  Initialize our sum to 0
    for(i = min; i < max; i++) { //  Loop from our minimum value to the maximum
        sum += i;                //  Increase our sum
    }
    return sum;                  //  Return the sum
}

int main(int argc, char** argv) {
    // Final sum
    unsigned long long finalSum;

    // Timing values
    clock_t startTime, stopTime;

    // Start the timer
    startTime = clock();

    // Get the maximum N
    unsigned long long maximum = processCommandLine(argc, argv);

    finalSum = sum_range(0, maximum); // Start from 0 and go to the maximum (from command line)

    stopTime = clock();

    printf("Final sum with 1 processor  in %f seconds is: %llu\n", \
            (double)(stopTime - startTime) / CLOCKS_PER_SEC, finalSum);

    return 0;
}

