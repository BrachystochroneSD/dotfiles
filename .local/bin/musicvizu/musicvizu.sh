#!/bin/sh


TERM=alacritty
MUSIC_DIR=$(grep music_dir ${HOME}/.config/mpd/mpd.conf | sed 's|.*"\(.*\)/".*|\1|;s/\\//g' | sed "s|~|${HOME}|")
BACKUP_IMG=${HOME}/.config/wpg/mywalls/owl.png #when no cover img found

get_image() {
    file=$1
    echo "$file" | grep -q acast && echo "/tmp/podcast" && return

    radio=$(grep "$file" "${HOME}/.config/music_radios")
    [ -n "$radio" ] && echo "${HOME}/.local/bin/musicvizu/radio_img/${radio%% *}.png" && return

    album_dir="${file%/*}"
    if [ -n "$album_dir" ] ; then
        album_dir="$MUSIC_DIR/$album_dir"
        covers="$(find "$album_dir" -type d -exec find {} -maxdepth 1 -type f -iregex ".*/.*\(${album}\|cover\|folder\|artwork\|front\).*[.]\(jpe?g\|png\|gif\|bmp\)" \; )"
        nosmall=$(echo "$covers" | grep -v "Small")
        [ -n "$nosmall" ] && covers="$nosmall"
        src="$(printf "$covers" | head -n1)"
        echo $src
    fi
}

get_source_image() {
    src=""
    album=$(mpc --format %album% current 2>/dev/null)
    file=$(mpc --format %file% current 2>/dev/null)

    if [ -n "$file" ]; then
        get_image "$file"
    else
        echo "$BACKUP_IMG"
    fi
}

exec $TERM -t cavaviz -e 'cava' &
feh -F --title album "$(get_source_image)" &
if [ -n "$1" ]; then
    ncmpcpp -s playlist -h "$1"
else
    ncmpcpp -s playlist
fi
