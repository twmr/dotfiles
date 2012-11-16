#!/bin/bash

echo "Port of active NetLib Servers"
grep "Binding service" *.log | awk -F ':' '{ print $8 }' | sort -n
