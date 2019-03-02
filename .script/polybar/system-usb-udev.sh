#!/bin/sh

usb_print() {
    devices=$(lsblk -Jplno LABEL,NAME,TYPE,RM,SIZE,MOUNTPOINT,VENDOR)
    output=""
    counter=0
    for unmounted in $(echo "$devices" | jq -r '.blockdevices[]  | select(.type == "part") | select(.rm == "1") | select(.mountpoint == null) | .name'); do
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

    for mounted in $(echo "$devices" | jq -r '.blockdevices[] | select(.type == "part") | select(.rm == "1") | select(.mountpoint != null) | .size'); do

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
        for mount in $(echo "$devices" | jq -r '.blockdevices[]  | select(.type == "part") | select(.rm == "1") | select(.mountpoint == null) | .name'); do

            mountpoint=$(udisksctl mount --no-user-interaction -b $mount)
            mountpoint=$(echo $mountpoint | cut -d " " -f 4 | tr -d ".")
            exec st -n ranger -e ranger $mountpoint &

            
        done
        usb_update


        ;;
    --unmount)
        devices=$(lsblk -Jplno NAME,TYPE,RM,MOUNTPOINT)

        for unmount in $(echo "$devices" | jq -r '.blockdevices[]  | select(.type == "part") | select(.rm == "1") | select(.mountpoint != null) | .name'); do
            sudo udisksctl unmount --no-user-interaction -b "$unmount"
            sudo udisksctl power-off --no-user-interaction -b "$unmount"
        done

        usb_update
        ;;
    *)
        trap exit INT
        trap "echo" USR1

        usb_print

        ;;
esac
