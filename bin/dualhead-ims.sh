#!/bin/sh

outleft=HDMI-0
outright=DVI-I-1
xrandr --output ${outleft} --auto
xrandr --output ${outright} --auto
xrandr --output ${outright} --right-of ${outleft}
xrandr --output ${outright} --rotate normal
