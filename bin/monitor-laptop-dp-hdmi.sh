#!/usr/bin/env bash
xrandr \
  --output eDP-1 --auto --left-of DP-1 \
  --output DP-1 --auto --left-of HDMI-1 --primary \
  --output HDMI-1 --auto
restart-apps.sh
