#!/bin/sh

outleft=DVI-2
outright=DVI-3
xrandr --output ${outleft} --auto
xrandr --output ${outright} --auto
xrandr --output ${outright} --right-of ${outleft}
