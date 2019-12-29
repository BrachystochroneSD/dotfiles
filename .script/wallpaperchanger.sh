#!/bin/sh

curr_wall=$(awk 'NR==2{print $3}' ${HOME}/.fehbg | sed "s/'//g" | sed 's\.*/\\g')

is_gif=$(echo "$curr_wall" | sed 's/.*.gif/true/g')

[ "$is_gif" = "true" ] &&

xwinwrap -ov -g 1366x768+0+0 -- mpv -wid WID "$image_path" --no-osc --no-osd-bar --loop-file --player-operation-mode=cplayer --no-audio --panscan=1.0 --no-input-default-bindings
