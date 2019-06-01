#!/bin/sh

exec st -n cavaviz -e cava &
exec st -n album -e ~/.script/musicvizu/albumshow.sh &
exec st -n ncmpcppwin -e ncmpcpp -s playlist &
