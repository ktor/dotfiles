#!/usr/bin/env bash
xrandr --auto --output eDP-1 --auto --output HDMI-2 --primary --right-of eDP-1
restart-apps.sh
