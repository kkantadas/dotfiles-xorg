#!/bin/sh

picom &
sh bin/acpiloop.sh &
sh ~/.fehbg &&  ~/bin/wallpaper.sh &
urxvtd -q -o -f &
#redshift &
syndaemon -i 1.0 -d -K &
xrdb -merge ~/.Xresources &
xsetroot -cursor_name left_ptr
#xbindings &
#sudo powertop --auto-tune
#xrandr -s 1280x1024 
#####xrandr --output eDP1 --off # sceen turn screen on -> xrandr --auto 
#xinput disable "AT Translated Set 2 keyboard" -> at /boot/loader/entries/arch.conf ->  i8042.nokbd
synclient TapButton1=1 #enable touch-klick
xinput set-prop 10 "libinput Middle Emulation Enabled" 1
sudo alsactl restore # restore alsamixer
sudo systemctl restart bluetooth
xmodmap .Xmodmap
sudo rfkill unblock bluetooth
# Fire it up
exec ratpoison
#exec dbus-launch --exit-with-session emacs -mm --debug-init
