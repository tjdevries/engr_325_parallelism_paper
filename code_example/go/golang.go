// For loop

package main

import (
    "fmt"
    "flag"
    "time"
)

func sum_range(min, max int, c chan int)  {
    var i int
    var temp_sum int

    temp_sum = 0

    for i = min; i < max; i++ {
        temp_sum += i
    }

    c <- temp_sum
}

func main() {
    var sum_sequential, sum_concurrent int
    var max int
    
    flag.IntVar(&max, "max", 10, "Number of integers to sum")
    flag.Parse()

    start_sequential := time.Now()

    c_seq := make(chan int)
    go sum_range(0, max, c_seq)
    sum_sequential = <- c_seq

    elapsed_sequential := time.Since(start_sequential)
    fmt.Printf("NumProc: %d, Time: %7.5f, Sum: %d\n", 1, float64(elapsed_sequential)/1000000000, sum_sequential)

    start_concurrent := time.Now()

    sum_concurrent = 0
    
    c1 := make(chan int)
    c2 := make(chan int)
    go sum_range(0, max/2, c1)
    go sum_range(max/2, max, c2)

    sum_concurrent = <- c1
    sum_concurrent += <- c2

    elapsed_concurrent := time.Since(start_concurrent)
    fmt.Printf("NumProc: %d, Time: %7.5f, Sum: %d\n", 2, float64(elapsed_concurrent)/1000000000, sum_concurrent)
}

