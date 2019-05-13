#!/usr/bin/env bash
xrandr \
  --output HDMI-2 --auto \
  --output DP-1 --auto --right-of HDMI-2 --primary \
  --output eDP-1 --auto --right-of DP-1
restart-apps.sh
