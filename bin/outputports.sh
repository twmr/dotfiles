#!/bin/bash
echo "Port of active NetLib Servers"
grep "Binding service" *.log | awk -F '*' '{ print $2 }' | cut -d: -f2 | sort -n
