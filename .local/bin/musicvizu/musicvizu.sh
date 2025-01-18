#!/bin/sh

winid=$(xprop -root _NET_ACTIVE_WINDOW | cut -d' ' -f5)

exec st 2>/dev/null -t cavaviz -e 'cava' &
exec st -t album -e ~/.local/bin/musicvizu/albumshow.sh & 2>/dev/null
sleep 0.1
wmctrl -i -a "$winid"
if [ -n "$1" ];then
    ncmpcpp -s playlist -h 192.168.0.103
else
    ncmpcpp -s playlist
fi
