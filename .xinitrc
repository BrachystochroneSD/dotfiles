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
redshift &
picom &

# nextcloud
nextcloud --background &

#emacs
emacs --daemon

. /etc/X11/xinit/xinitrc.d/50-systemd-user.sh
exec dwm
