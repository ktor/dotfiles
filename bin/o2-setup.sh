xrandr --output eDP1 --auto --output HDMI2 --auto --left-of VGA1 --output VGA1 --left-of eDP1 --primary
pkill -f stalonetray
stalonetray &
