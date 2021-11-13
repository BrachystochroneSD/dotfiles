#!/bin/sh

exec st 2>/dev/null -n cavaviz -e 'cava' &
exec st -n album -e ~/.script/musicvizu/albumshow.sh & 2>/dev/null
sleep 0.1
if [ -n "$1" ];then
    st -n ncmpcppwin -e ncmpcpp -s playlist -h 192.168.0.103
else
    st -n ncmpcppwin -e ncmpcpp -s playlist
fi
