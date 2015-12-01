#!/bin/bash

echo "================================================================================" > final_results.txt
echo "                      Final Results, N = $1" >> final_results.txt
echo "================================================================================" >> final_results.txt

for D in `find . -type d`
do
    if [[ $D != '.' ]]; then
        echo "Entering $D"
        cd $D
        if [[ $D == './erlang' ]]; then
            TEMP=$(./run.sh 1000000)
        else
            ./run.sh $1
            cat result.txt >> ../final_results.txt        
        fi
        cd ..
    fi
done

echo "$TEMP" >> final_results.txt
