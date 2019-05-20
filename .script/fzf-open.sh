#!/bin/sh


case $1 in
    --hidden)
	shit=$(find . -type f | fzf --layout=reverse --height 40% )
	;;
    *)
	shit=$(fzf --layout=reverse --height 40%)
	;;
esac
if [ -n "$shit" ];then
    nohup xdg-open "$shit" >/dev/null 2>&1 &
    echo "Opening $shit"
    sleep 1
fi

