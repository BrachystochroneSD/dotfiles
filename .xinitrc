# Launch program
setxkbmap -layout be

# colors background
wpg -m

# binding keys
xbindkeys

# music shit
mpd

# redshift
redshift &
picom &

# nextcloud
nextcloud --background &

. /etc/X11/xinit/xinitrc.d/50-systemd-user.sh
exec i3

