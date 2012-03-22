#!/bin/sh

outleft=HDMI-0
outright=DVI-0
xrandr --output ${outleft} --auto
xrandr --output ${outright} --auto
xrandr --output ${outright} --right-of ${outleft}
