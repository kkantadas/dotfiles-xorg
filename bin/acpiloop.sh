#!/bin/bash

while true
do

grep -q 0 /sys/class/power_supply/ACAD/subsystem/BAT1/capacity
  if [ $? = 0 ]; then

sleep 0.5
else
 
   for ((;;))
     do
       grep -q 0 /sys/class/power_supply/ACAD/online
    if [ $? = 0 ]
       then
  ## Laptop is not pluged in
         ratpoison -c "echo ...Laptop is  NOT  pluged in..." &
	# ratpoison -d :0.0 -c "echo `acpi -b`" &
     fi
        sleep 2m
        done

   fi
done
