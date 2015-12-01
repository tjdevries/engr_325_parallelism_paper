use Time;

// Read in the command line argument, if passed
config var maximum: int = 10;

// Initialize our sum variable
var sum:int = 0;

// Declare and start our timer
var t:Timer;
t.start();

// Loop through until we have processed (maximum) number
for i in {0..(maximum-1)} {
    // Increment our sum
    sum = sum + i;
}

t.stop();

// Write out the result
writef("NumProc: 1, Time: %7.7r, Sum: %i\n", t.elapsed(), sum);
