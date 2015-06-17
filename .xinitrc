#!/usr/bin/env bash

DEFAULT_SESSION="xmonad"
BGCOLOUR="#101010"
WALLPAPER="~/.wallpaper.png"

#Fix libreoffice problems
export OOO_FORCE_DESKTOP="none"
#Fix SDL Problems
export SDL_VIDEO_FULLSCREEN_HEAD=0

#Set our displays up
#xrandr --output DVI-I-1 --mode 1920x1200 --pos 0x0 --output DVI-I-2 --mode 1920x1200 --pos 1920x0

setxkbmap gb &
set m 5 &
xset -b &
imwheel &
xrdb -merge ~/.Xdefaults &
xsetroot -cursor_name left_ptr &

if [[ -f $WALLPAPER ]]; then
    hsetroot -center $WALLPAPER &
else
    xsetroot -solid $BGCOLOUR &
fi

if [ -z "$1" ] ; then
    SESSION=$DEFAULT_SESSION
else
    SESSION="$1"
fi

case $SESSION in
dwm)
    wmname LG3D &
    ;;
fluxbox)
    xbindkeys &
    ;;
openbox)
#    tint2 &
#    xbindkeys &
    ;;
xmonad)
    wmname LG3D &
    ;;
gnome)
    SESSION="ck-launch-session gnome-session"
    ;;
exn)
    xterm &
    xbindkeys &
    ;;
wmii)
    wmname LG3d &
    xbindkeys &
    ;;
stumpwm)
    ;;
esac

exec $SESSION
