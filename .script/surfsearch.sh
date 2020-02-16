#!/bin/sh

# colors from wpgtk/pywal
. "${HOME}/.config/wpg/formats/colors.sh"

echo | dmenu -nb "$color0" -nf "$color15" -sb "$color0" -sf "$color3" -i -p "Surf Search" | xargs -I /home/sam/.config/surf-2.0/surf-open.sh 
