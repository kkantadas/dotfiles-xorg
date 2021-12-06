#!/bin/sh

# Change file size on the flight in urxvt


printf '\033]10;#f0f0f0\007'
printf '\033]11;#3f3f3f\007'
exec transset-df -a .9 > /dev/null  
