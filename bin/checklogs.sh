#!/bin/bash

for i in *err.log; do
    echo $i
    cat $i
    echo ""
    echo ""
done

echo
echo "Checking for active Servers"
grep Binding *.log
