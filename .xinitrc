# Launch program
setxkbmap -layout be
numlockx

# colors background
wpg -m

# binding keys
xbindkeys

# music shit
#mpd

# redshift
unclutter -idle 1 -jitter 2 -root &
redshift &
picom &
dwmblocks &

# nextcloud
nextcloud --background &

#emacs
emacs --daemon

#custom shit
~/.script/polybar/wifi --load-config

. /etc/X11/xinit/xinitrc.d/50-systemd-user.sh


${HOME}/.script/execdwm
#exec dwm 2> ${HOME}/.dwm.log
