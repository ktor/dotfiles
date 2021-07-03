#!/usr/bin/env bash
xrandr \
  --output HDMI-1 --auto --above eDP-1 \
  --output eDP-1 --auto --primary
restart-apps.sh
