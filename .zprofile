# FZF
export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"
#
# android
export ANDROID_SDK_ROOT='/opt/android-sdk/'
export _JAVA_AWT_WM_NONREPARENTING=1

export EDITOR="/usr/bin/vim"

export PATH=$PATH:~/.local/bin

export TEXMFHOME="~/.config/texmf"

#export XDG_CONFIG_HOME="~/.config"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi
