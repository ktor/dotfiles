#!/usr/bin/env bash
xrandr \
  --output HDMI-1 --auto --left-of eDP-1 \
  --output eDP-1 --auto --primary
restart-apps.sh
