#!/bin/bash

ariaconf=${HOME}/.config/aria2/aria2.bittorrent 
torrentdir=${HOME}/.config/aria2/torrents
musicdir=${HOME}/Music
videodir=${HOME}/Movies
dldir=${HOME}/Downloads

downloading(){
    magnetlink=$1

    type=$(echo -e "Music\nVideo\nFile" | dmenu -p "What's the type of the download?" -l 3) || exit 1
    if [[ $type == 'Music' ]]; then
	savedir=$(find $musicdir -maxdepth 1 | dmenu -l 10 -i) || exit 1
    elif [[ $type == 'Video' ]]; then
	savedir=$videodir
    else
	savedir=$dldir
    fi

    exec st -n ariawin -e aria2c --conf-path $ariaconf --dir $savedir $magnetlink &
    echo -e "Waiting for the torrent file creation...\nPress key to continue"
    read -ern 1


    torrentfile=$(ls "$savedir" | grep .torrent) || (echo "no torrent found" ; exit 1)

    name=$(aria2c --show-files $savedir/$torrentfile | grep -i name | sed 's/.*: //' | sed 's/ /_/g')
    echo $name

    ln -s "$savedir" "$torrentdir"/"$name"
    mv "$savedir"/"$torrentfile" "$torrentdir"/"$name".torrent
}

seeding(){
    for tfile in $(ls $torrentdir | grep .torrent); do
	dir=$(readlink $torrentdir/$(echo $tfile | sed 's/\.torrent//'))
	exec st -n ariawin -e aria2c --conf-path "$ariaconf" -d "$dir" "$torrentdir"/"$tfile" &
    done
}


if ping -q -c 1 -W 1 1.1.1.1 &>/dev/null; then
    while getopts ":sd:" opt; do
	case $opt in
	    d)
		downloading $OPTARG
		;;
	    s)
		seeding
		;;
	    :)
		echo "Need an options"
		;;
	esac
    done
else
    echo "No internet connection"
fi
