#!/usr/bin/env bash
xrandr \
  # --output DP-1 --auto  --primary \
  # --output eDP-1 --auto --right-of DP-1
xrandr \
  --output HDMI-1 --auto \
  --output DP-1 --auto --right-of HDMI-1 --primary \
  --output eDP-1 --auto --right-of DP-1
restart-apps.sh
