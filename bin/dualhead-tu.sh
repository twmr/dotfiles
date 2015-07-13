#!/bin/sh

outleft=HDMI1
outright=HDMI2
xrandr --output ${outleft} --auto
xrandr --output ${outright} --auto
xrandr --output ${outright} --right-of ${outleft}
xrandr --output ${outright} --rotate normal
