# Launch program
setxkbmap -layout be
numlockx

# colors background
wpg -m

xbindkeys
unclutter -idle 1 -jitter 2 -root &
redshift &
# picom &
dwmblocks &
nextcloud --background &
emacs --daemon

. /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

${HOME}/.local/bin/execdwm
