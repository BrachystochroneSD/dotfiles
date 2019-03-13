#!/bin/bash

# colors from wpgtk/pywal
. "${HOME}/.config/wpg/formats/colors.sh"

ariaconf=${HOME}/.config/aria2/aria2.bittorrent 
torrentdir=${HOME}/.config/aria2/torrents
musicdir=${HOME}/Music
videodir=${HOME}/Movies
dldir=${HOME}/Downloads

downloading(){

    magnetlink=$1
    i=0
    type=$(echo -e "Music\nVideo\nFile" | dmenu -nb "$color0" -nf "$color15" -sb "$color0" -sf "$color3" -i -p "What's the type of the download?" -l 3) || exit 1

    ######################################
    # Downloads Directory change by type #
    ######################################

    if [[ $type == 'Music' ]]; then
	savedir=$(find $musicdir -maxdepth 1 | dmenu -nb "$color0" -nf "$color15" -sb "$color0" -sf "$color3" -i -l 10 -i) || exit 1
    elif [[ $type == 'Video' ]]; then
	savedir=$videodir
    else
	savedir=$dldir
    fi

    exec st -n ariawin -e aria2c --conf-path $ariaconf --dir $savedir $magnetlink &
    echo "Waiting for the torrent file creation"

    while [ ! -f $savedir/*.torrent ]; do
	sleep 1
	printf "."
	[[ $i -ge 30 ]] && break
	((i++))
    done
    
    echo -e "\nTorrent file found, processing the datas for futur seeding \n \n"    
    torrentfile=$(ls "$savedir" | grep .torrent) && echo "Torrent file is : $torrentfile" || (echo "no torrent found" ; exit 1)

    name=$(aria2c --show-files $savedir/$torrentfile | grep -i name | sed 's/.*: //' | sed 's/ /_/g')
    echo "Name is " $name

    ln -s "$savedir" "$torrentdir"/"$name"
    mv "$savedir"/"$torrentfile" "$torrentdir"/"$name".torrent
    echo -e "\n\nDone"
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
