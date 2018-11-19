xrandr --auto --output eDP1 --auto --output HDMI2 --primary --right-of eDP1
pkill -f stalonetray
stalonetray &
