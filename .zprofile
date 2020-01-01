# export EDITOR=vim
# export VISUAL=/home/sam/.script/edit
# export BROWSER=firefox

#######
# FZF #
#######

export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi

