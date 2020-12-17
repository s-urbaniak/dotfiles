#!/bin/sh
xrandr --output DP1 --off --output eDP1 --scale "$1"x"$1" --primary --mode 3840x2160 --pos 0x0 --rotate normal --output DP1 --off
