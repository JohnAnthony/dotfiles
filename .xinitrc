xset -dpms &
hsetroot -center ~/.wallpaper &

# Update status every second
while true; do xsetroot -name "`date`"; sleep 1; done &

exec /home/john/bin/dwm
