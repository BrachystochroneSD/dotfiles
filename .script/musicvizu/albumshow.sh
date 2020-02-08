#!/bin/sh

w3m="w3mimgdisplay"
MUSIC_DIR=$(grep music_dir ${HOME}/.config/mpd/mpd.conf | sed 's|.*"\(.*\)/".*|\1|;s/\\//g' | sed "s|~|${HOME}|")
backup_img=${HOME}/.config/wpg/mywalls/owl.png #when no cover img found

while true;do
    src=""
    album=$(mpc --format %album% current)
    file=$(mpc --format %file% current)
    album_dir="${file%/*}"
    if [ -n "$album_dir" ] ; then
	album_dir="$MUSIC_DIR/$album_dir"
	covers="$(find "$album_dir" -type d -exec find {} -maxdepth 1 -type f -iregex ".*/.*\(${album}\|cover\|folder\|artwork\|front\).*[.]\(jpe?g\|png\|gif\|bmp\)" \; )"
	src="$(printf "$covers" | head -n1)"
    fi
    [ -z "$src" ] && src=$backup_img

    # Display the image.
    printf '0;1;0;0;192;192;;;;;%s\n3;\n4\n' \
           "$src" | "$w3m" >/dev/null 2>&1

    sleep 1

done
