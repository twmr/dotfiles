#!/bin/sh

outleft=DVI-I-1
outright=DVI-I-2
xrandr --output ${outleft} --auto
xrandr --output ${outright} --auto
xrandr --output ${outright} --right-of ${outleft}
xrandr --output ${outright} --rotate left
