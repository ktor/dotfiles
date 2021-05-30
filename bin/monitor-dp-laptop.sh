#!/usr/bin/env bash
xrandr \
  --output DP-1-1 --auto --left-of eDP-1-1 \
  --output eDP-1-1 --auto --primary
restart-apps.sh
