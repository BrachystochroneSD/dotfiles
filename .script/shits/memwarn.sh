#!/bin/sh

while true; do
    memstil=$(~/.script/polybar/memram | grep -o "[0-9]*")
    [ $memstil -gt 90 ] && paplay /usr/share/sounds/freedesktop/stereo/bell.oga

    sleep 1
done

