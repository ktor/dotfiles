#!/usr/bin/env bash
xrandr \
  --output eDP-1 --auto --left-of DP-1-1 \
  --output DP-1-1 --auto --primary
restart-apps.sh
