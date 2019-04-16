#!/usr/bin/env bash
xrandr \
  --output HDMI2 --auto \
  --output DP1 --auto --right-of HDMI2 --primary \
  --output eDP1 --auto --right-of DP1
restart-apps.sh
