#!/bin/bash

SET_INT_PROP="xinput set-prop --type=int --format=8"

if [ $(id -u) = "0" ]; then
    echo "superuser"

    #dirty writeback time
    echo 1500 > /proc/sys/vm/dirty_writeback_centisecs

    #enable wireless powersaving
    #read status with iwpriv eth1 get_power
    iwpriv eth1 set_power 5

    exit 0;
fi

#enable horizontal and vertical scrolling
$SET_INT_PROP "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1
$SET_INT_PROP "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2
#$SET_INT_PROP "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200
$SET_INT_PROP "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5

synclient TapButton1=1
synclient HorizTwoFingerScroll=1
