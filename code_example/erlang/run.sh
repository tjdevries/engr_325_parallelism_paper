#!/bin/bash

# make

echo "Erlang Results:" > result.txt

erl -noshell -eval "sequential_sum:print_sum($1,0)" -s init stop >> result.txt
erl -noshell -eval "concurrent_sum:find_total_sum($1, 2)" -s init stop >> result.txt

cat result.txt
