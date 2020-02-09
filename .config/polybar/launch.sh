#!/bin/bash

. ${HOME}/.cache/wal/colors.sh

colorbg=$(echo "$color0" | sed 's/#//')
export colorbg_alow="#cc$colorbg"
export colorbg_amedium="#ee$colorbg"

# This script change the base16-eval-theme for emacs generated by pywal with the template. Because the base color is too bright. It also copy the templates for dmenu shits for homogenous colors

# Terminate already running bar instances
killall -q polybar
# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
polybar Bar &


# polybar Barbg & polybar Bar1 & polybar Bar2 & polybar Bartray
