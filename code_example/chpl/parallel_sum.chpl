use Time;

// Read in the command line argument, if passed
config var maximum: int = 10;
config var numProc: int = 1;

// Get the lsit of processor IDs
var processor_list: domain(1) = 1..numProc;

proc sum_range(min, max) {
    var sum: int = 0;
    var i: int = min;
    while (i < max) {
        sum = sum + i;
        i += 1;
    }
    return sum;
}

// Create the timer
var t:Timer;

// Start the timer
t.start();

// Loop through until we have processed (maximum) number
var sum = + reduce ([i in processor_list] sum_range( (i - 1) * maximum / numProc, i * maximum / numProc ));

// Finish the timer
t.stop();

// Write out the result
writef("NumProc: %i, Time: %7.7r, Sum: %i\n", numProc, t.elapsed(), sum);
