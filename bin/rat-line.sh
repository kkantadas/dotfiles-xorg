#!/bin/bash

exec ratpoison -d :0.0 -c "echo `~/bin/ping-check.sh ` Net: `cat /sys/class/net/enp2s0f1/operstate` @  Wifi: `cat /sys/class/net/wlan0/operstate /sys/class/net/wlp3s0/operstate` `iwconfig wlan0 |grep ESSID|cut -c30-50` @ Bat:`acpi -b |cut -c11-27` @  Mem: `free -h | grep Mem | cut -c27-30` @ Themp:`acpi -t|cut -c15-19`C @ `date|cut -c11-16` `date|cut -c1-10`"

