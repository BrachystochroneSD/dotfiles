#!/bin/sh

pgrep -x transmission-da >/dev/null || (transmission-daemon && notify-send "Starting transmission deamon" && sleep 2) 

transmission-remote -a "$@" && notify-send "Torrent added"
