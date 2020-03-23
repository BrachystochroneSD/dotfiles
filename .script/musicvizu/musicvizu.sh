#!/bin/sh


exec st -n cavaviz -e cava &
pid="$pid\n$!"
exec st -n album -e ~/.script/musicvizu/albumshow.sh &
pid="$pid\n$!"
[ -n "$1" ] && ncmpcpp -s playlist -h 192.168.1.103 || ncmpcpp -s playlist

echo -e "$pid" | tail -n2 | while read a;do kill $a;done
