# Launch program
setxkbmap -layout be
numlockx

# colors background
wpg -m

xbindkeys
unclutter -idle 1 -jitter 2 -root &
redshift &
picom --experimental-backends &
dwmblocks &
nextcloud --background &
emacs --daemon

#custom shit
~/.script/polybar/wifi --load-config
. /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

${HOME}/.script/execdwm
