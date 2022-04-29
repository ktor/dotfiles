#!/usr/bin/env bash
xrandr \
  --output DP-1-8 --auto --right-of eDP-1 \
  --output eDP-1 --auto --primary
restart-apps.sh
