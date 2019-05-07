#!/bin/bash

# colors from wpgtk/pywal
. "${HOME}/.config/wpg/formats/colors.sh"

choice=$(echo -e "LIVE\nGAME\nVOD" | dmenu -nb "$color0" -nf "$color15" -sb "$color0" -sf "$color3" -i -l 10 -p "Twitch Menu :")

if ping -q -c 1 -W 1 1.1.1.1 &>/dev/null; then
    case $choice in
	LIVE)
	    ${HOME}/.script/twitch/twitchscript --live
	    ;;
	GAME)
	    ${HOME}/.script/twitch/twitchscript --game
	    ;;
	VOD)
	    ${HOME}/.script/twitch/twitchscript --vod
	    ;;
	*)
	    exit 0
	    ;;
    esac
else
    echo "No internet connection"
fi

