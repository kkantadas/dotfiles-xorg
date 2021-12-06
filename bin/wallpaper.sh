#!/bin/bash

shopt -s nullglob
#cd ~/bin/wallpapers
cd ~/bin/wallpapger-1280x1024
#cd ~/downloads
while true; do
        files=()
        for i in *.jpg *.png *.JPG *.jpeg; do
                [[ -f $i ]] && files+=("$i")
        done
        range=${#files[@]}

        ((range)) && feh --bg-scale "${files[RANDOM % range]}"

        sleep 4.5m

done
