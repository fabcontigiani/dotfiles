#!/bin/sh
xrandr --output HDMI-1 --auto --output HDMI-2 --auto --left-of HDMI-1 &
picom &
megasync &
