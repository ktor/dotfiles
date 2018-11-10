xrandr --output eDP1 --auto --primary --output HDMI2 --right-of eDP1
pkill -f stalonetray
stalonetray &
