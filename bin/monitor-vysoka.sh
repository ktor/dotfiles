#!/usr/bin/env bash
xrandr --output eDP-1 --auto --output HDMI-2 --auto --right-of eDP-1 --primary
restart-apps.sh
