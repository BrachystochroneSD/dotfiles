#!/bin/sh

case $1 in
    --open-with)
        bin=$(ls /usr/bin | fzf)
        shit=$(fzf --layout=reverse --height 40%)
        ;;
    --hidden)
        shit=$(find . -type f | fzf --layout=reverse --height 40% )
    ;;
    *)
        shit=$(fzf --layout=reverse --height 40%)
    ;;
esac

if [ -n "$shit" ];then
    if [ -n "$bin" ];then
        nohup $bin "$shit" >/dev/null 2>&1 &
    else
        nohup xdg-open "$shit" >/dev/null 2>&1 &
    fi
    echo "Opening $shit"
    sleep 1
fi

