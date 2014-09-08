#!/bin/sh

outleft=LVDS1
outright=VGA1
xrandr --output ${outleft} --auto
xrandr --output ${outright} --auto
xrandr --output ${outright} --right-of ${outleft}
xrandr --output ${outright} --rotate normal
