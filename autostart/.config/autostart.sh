#!/bin/sh

autorandr -c &
picom &
lxsession &
dunst &
xss-lock --transfer-sleep-lock -- i3lock -i /home/fab/.config/wallpaper.png &
unclutter &
nm-applet &
thunar --daemon &
transmission-daemon
