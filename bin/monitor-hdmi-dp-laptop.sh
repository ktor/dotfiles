#!/usr/bin/env bash
xrandr \
  --output HDMI-1-1 --auto \
  --output DP-1-1 --auto --right-of HDMI-1-1 --primary \
  --output eDP-1-1 --auto --right-of DP-1-1
restart-apps.sh
