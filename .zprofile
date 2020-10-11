#######
# FZF #
#######

export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"

export TEXMFHOME="~/.config/texmf"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi
