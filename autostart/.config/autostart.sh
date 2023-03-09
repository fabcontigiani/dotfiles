#!/bin/sh

autorandr -c &
lxsession &
dunst &
picom &
unclutter &
thunar --daemon &
emacs --daemon &
transmission-daemon
