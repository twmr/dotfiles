#!/bin/bash

BT_RFKILL=$(rfkill list | grep tpacpi_bluetooth_sw | sed 's/\([0-9]\+\):.*/\1/')
BT_STATE=$(/sbin/rfkill list $BT_RFKILL | grep "Soft blocked: yes")

if [ "x" == "x$BT_STATE" ]; then
	/sbin/rfkill block $BT_RFKILL
else
	/sbin/rfkill unblock $BT_RFKILL
fi

exit 0
