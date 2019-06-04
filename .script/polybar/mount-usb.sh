#!/bin/bash

# colors from wpgtk/pywal
. "${HOME}/.config/wpg/formats/colors.sh"

dmenucmd="dmenu -nb $color0 -nf $color15 -sb $color0 -sf $color3"
dmenuobf="dmenu -nb $color0 -nf $color0 -sb $color0 -sf $color3"

devices=$(lsblk -Jplno NAME,TYPE,RM,MOUNTPOINT)

usb_print() {
    devices=$( lsblk -Jplno LABEL,NAME,TYPE,RM,SIZE,MOUNTPOINT,VENDOR)
    output=""
    counter=0
    for unmounted in $(echo "$devices" | jq -r '.blockdevices[]  | select(.type == "part") | select(.rm == true) | select(.mountpoint == null) | .name'); do
	unmounted=$(echo "$unmounted" | tr -d "[:digit:]")
        unmounted=$(echo "$devices" | jq -r '.blockdevices[]  | select(.name == "'"$unmounted"'") | .vendor')
        unmounted=$(echo "$unmounted" | tr -d ' ')

        [ $counter -eq 0 ] && space="" || space=" "
        counter=$((counter + 1))

        output=" $output$space$unmounted "
    done

    for mounted in $(echo "$devices" | jq -r '.blockdevices[] | select(.type == "part") | select(.rm == true) | select(.mountpoint != null) | .size'); do

        if [ $counter -eq 0 ]; then
            space=""
        else
            space=" "
        fi
        counter=$((counter + 1))

        output="$output$space:$mounted"
    done

    echo "$output"
}

case "$1" in
    --mount)
        for mount in $(echo "$devices" | jq -r '.blockdevices[]  | select(.type == "part") | select(.rm == true) | select(.mountpoint == null) | .name'); do
	    [ -n "$(ls /media/usb_drive1/)" ] && num=2 || num=1
	    prompt=$(printf "Ye\nNah" | $dmenucmd -p "Mounting $mount on /media/usb_drive$num?")
	    if [ "$prompt" = "Ye" ] ; then
		echo | $dmenuobf -p "[sudo] password for $USER" | sudo -S mount "$mount" /media/usb_drive$num/ -o uid="$USER" -o gid="$(id -gn "$USER")" && exec st -n fff -e fff /media/usb_drive$num/ || dunstify -i owl "Wrong password or usb is in use"
	    fi
        done
        ;;

    --unmount)
        for unmount in $(echo "$devices" | jq -r '.blockdevices[]  | select(.type == "part") | select(.rm == true) | select(.mountpoint != null) | .mountpoint'); do
	    prompt=$(printf "Ye\nNah" | $dmenucmd -p "Unmount $unmount?")
	    if [ "$prompt" = "Ye" ] ;then
		echo | $dmenuobf -p "[sudo] password for $USER" | sudo -S umount "$unmount" || dunstify -i owl "Wrong password"
	    fi
        done
        ;;

    *)
        usb_print
        ;;
esac
