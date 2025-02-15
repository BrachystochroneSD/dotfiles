# Theming
export QT_QPA_PLATFORMTHEME="qt6ct"
export GTK_THEME="Arc-Dark"
export GTK3_RC_FILES="$HOME/.config/gtk-3.0/settings.ini"

# GMB
export GBM_BACKEND=nvidia-drm
export __GLX_VENDOR_LIBRARY_NAME=nvidia

# FZF
export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"

# android
export ANDROID_SDK_ROOT='/opt/android-sdk/'
export _JAVA_AWT_WM_NONREPARENTING=1

export EDITOR="/usr/bin/vim"

export PATH=~/.local/bin:$PATH

export TEXMFHOME="~/.config/texmf"

#export XDG_CONFIG_HOME="~/.config"

if test ! "$DISPLAY" && test "$XDG_VTNR" -eq 1; then
    if uwsm check may-start; then
        exec uwsm start hyprland.desktop
    fi
fi
