#!/bin/bash

echo "Golang Results:" > result.txt
./golang -max $1 >> result.txt

cat result.txt
