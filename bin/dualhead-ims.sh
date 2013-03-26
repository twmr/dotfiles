#!/bin/sh

outleft=DVI-I-2
outright=DVI-I-3
xrandr --output ${outleft} --auto
xrandr --output ${outright} --auto
xrandr --output ${outright} --right-of ${outleft}
