#!/bin/sh

# colors from wpgtk/pywal
. "${HOME}/.config/wpg/formats/colors.sh"

usb_print() {
    devices=$( lsblk -Jplno LABEL,NAME,TYPE,RM,SIZE,MOUNTPOINT,VENDOR)
    output=""
    counter=0
    for unmounted in $(echo "$devices" | jq -r '.blockdevices[]  | select(.type == "part") | select(.rm == true) | select(.mountpoint == null) | .name'); do
	unmounted=$(echo "$unmounted" | tr -d "[:digit:]")
        unmounted=$(echo "$devices" | jq -r '.blockdevices[]  | select(.name == "'"$unmounted"'") | .vendor')
        unmounted=$(echo "$unmounted" | tr -d ' ')

        if [ $counter -eq 0 ]; then
            space=""
        else
            space=" "
        fi
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

usb_update() {
    pid=$(pgrep -xf "/bin/sh /home/samrenfou/.script/system-usb-udev.sh")

    if [ "$pid" != "" ]; then
        kill -10 "$pid"
    fi
}

case "$1" in
    --update)
        usb_update
        ;;

    --mount)
        devices=$(lsblk -Jplno NAME,TYPE,RM,MOUNTPOINT)
        for mount in $(echo "$devices" | jq -r '.blockdevices[]  | select(.type == "part") | select(.rm == true) | select(.mountpoint == null) | .name'); do
	    [[ -n $(ls /media/usb_drive1/) ]] && num=2 || num=1
	    echo "" | dmenu -nb "$color0" -nf "$color0" -sb "$color0" -sf "$color3" -p "sudo password for mount $mount on /media/usb_drive$num/" | sudo -S mount $mount /media/usb_drive$num/
	    exec st -n fff -e fff /media/usb_drive$num/
        done
        usb_update
        ;;

    --unmount)
        devices=$(lsblk -Jplno NAME,TYPE,RM,MOUNTPOINT)

        for unmount in $(echo "$devices" | jq -r '.blockdevices[]  | select(.type == "part") | select(.rm == true) | select(.mountpoint != null) | .mountpoint'); do
	    echo "" | dmenu -nb "$color0" -nf "$color0" -sb "$color0" -sf "$color3" -p "sudo password for umount $unmount" | sudo -S umount $unmount
        done

        usb_update
        ;;

    *)
        trap exit INT
        trap "echo" USR1

        usb_print

        ;;
esac
