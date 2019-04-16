#!/usr/bin/env bash
xrandr --output eDP1 --auto --output HDMI2 --auto --right-of eDP1 --primary
restart-apps.sh
