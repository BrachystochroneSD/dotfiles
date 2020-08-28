#!/bin/bash

source "`ueberzug library`"

MUSIC_DIR=$(grep music_dir ${HOME}/.config/mpd/mpd.conf | sed 's|.*"\(.*\)/".*|\1|;s/\\//g' | sed "s|~|${HOME}|")
backup_img=${HOME}/.config/wpg/mywalls/owl.png #when no cover img found

ImageLayer 0< <(
    while true;do
        src=""
        album=$(mpc --format %album% current 2>/dev/null)
        file=$(mpc --format %file% current 2>/dev/null)
        album_dir="${file%/*}"
        if [ -n "$album_dir" ] ; then
            album_dir="$MUSIC_DIR/$album_dir"
            covers="$(find "$album_dir" -type d -exec find {} -maxdepth 1 -type f -iregex ".*/.*\(${album}\|cover\|folder\|artwork\|front\).*[.]\(jpe?g\|png\|gif\|bmp\)" \; )"
            nosmall=$(echo "$covers" | grep -v "Small")
            [ -n "$nosmall" ] && covers="$nosmall"
            src="$(printf "$covers" | head -n1)"
        fi
        [ -z "$src" ] && src=$backup_img

        # Display the image. TODO

        ImageLayer::add [identifier]="preview" \
                        [x]="0" \
                        [y]="0" \
                        [max_width]="19" \
                        [path]="$src"
        sleep 1
    done
    ImageLayer::remove [identifier]="preview"
)
