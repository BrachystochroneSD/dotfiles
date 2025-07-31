#!/bin/sh


TERM=alacritty
MUSIC_DIR=$(grep music_dir ${HOME}/.config/mpd/mpd.conf | sed 's|.*"\(.*\)/".*|\1|;s/\\//g' | sed "s|~|${HOME}|")
BACKUP_IMG=${HOME}/.config/mywalls/owl.png

kill_loop() {
    [ -n "$LOOP_PID" ] && kill "$LOOP_PID"
    rm /tmp/musicvizu_album
}

trap "kill_loop" TERM EXIT

get_image() {
    file=$1
    echo "$file" | grep -q acast && echo "/tmp/podcast" && return

    radio=$(grep "$file" "${HOME}/.config/music_radios")
    [ -n "$radio" ] && echo "${HOME}/.local/bin/musicvizu/radio_img/${radio%% *}.png" && return

    album_dir="${file%/*}"
    [ -z "$album_dir" ] && return 1

    album_dir="$MUSIC_DIR/$album_dir"
    covers="$(find "$album_dir" -type d -exec find {} -maxdepth 1 -type f -iregex ".*/.*\(${album}\|cover\|folder\|artwork\|front\).*[.]\(jpe?g\|png\|gif\|bmp\)" \; )"
    nosmall=$(echo "$covers" | grep -v "Small")
    [ -n "$nosmall" ] && covers="$nosmall"
    printf "$covers" | head -n1
}

get_source_image() {
    album=$(mpc --format %album% current 2>/dev/null)
    file=$(mpc --format %file% current 2>/dev/null)

    [ -n "$file" ] && image=$(get_image "$file")
    [ -z "$image" ] && image="$BACKUP_IMG"
    echo "$image"
}

check_file() {
    while true; do
        sleep 1
        cp "$(get_source_image)" /tmp/musicvizu_album
    done
}

cp "$(get_source_image)" /tmp/musicvizu_album

exec $TERM -t cavaviz -e 'cava' &
check_file &
LOOP_PID=$!

feh -F --title album "/tmp/musicvizu_album" &
if [ -n "$1" ]; then
    ncmpcpp -s playlist -h "$1"
else
    ncmpcpp -s playlist
fi

kill_loop
