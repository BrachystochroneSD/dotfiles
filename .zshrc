##############
# ZSH CONFIG #
##############

SAVEHIST=10
HISTFILE=~/.zsh_history

local WORDCHARS='*?_[]~=&;!#$%^(){}<>'

bindkey -e

autoload -U colors && colors

PS1="%B%{$fg[yellow]%}%n%{$fg[blue]%}@%{$fg[red]%}%M%{$fg[blue]%}:%{$fg[magenta]%}%~%{$reset_color%}$%b "

stty -ixon

# COMPLETION

autoload -U compinit
# zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
compinit
_comp_options+=(globdots)

# completion functions
_xdd() {
    local state
    _arguments '1: :->appli' '2: :->file'

    case $state in
        appli) _files -W "(/usr/share/applications ${HOME}/.local/share/applications)" -g "*.desktop" ;;
        file) _files ;;
    esac
}

_wifi() {
    _arguments -C \
               "--load-config[load configuration in ${home}/.config/wifirc]" \
               "--setup-zeros[setup the zero of the rx ]" \
               "--prompt-zeros[setup the zero of the rx with dmenu prompt]" \
               "--toggle-ifstat[toggle the print of the ifstat]"
}

compdef _xdd xdd
compdef _wifi wifi

# fix for tramp emacs
[ $TERM = "dumb" ] && unsetopt zle $$ PS1='$ '

##################
# Basics Aliases #
##################

alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -halF'
alias l='ls -CF'

alias untar='tar -xvf'
alias :c='cd ..'
alias drag='dragon-drag-and-drop --and-exit'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias diff='diff --color=auto'

alias ska="sudo killall"

alias xcb='xclip -selection clipboard'

alias cpr='cp -rv'
alias ytb2mp3='youtube-dl -x --embed-thumbnail --audio-format mp3'
alias filesize='du -h --max-depth=1 | sort -hr'
alias fsz='filesize'

##########
# PACMAN #
##########

alias sp='sudo pacman'
alias sps='sp -S'
alias spu='sp -Syyu'

############
# ZENOCYNE #
############

#zenohost
alias zenohost="sudo ${HOME}/.script/hostszeno"

#zenocloud
alias zenomount='sudo mount -t davfs https://nextcloud.zenocyne.com/remote.php/webdav/ ${HOME}/zenocloud'

##################
# Custom scripts #
##################

alias twitch='~/.script/twitch/twitchscript'
alias wifi='~/.script/polybar/wifi'
alias gamedbupdate='~/.script/gamedatabase/gamedbupdate'
alias yts='~/.script/youtube/youtubesearch'
alias im='~/.script/im'
alias xmen='~/.script/xrandrsw'
xop () { nohup xdg-open "$1" >/dev/null 2>&1 &}
alias fop='~/.script/fzf-open.sh'

#######
# GIT #
#######

alias dotfiles_git='/bin/git --git-dir=${HOME}/.dotfiles --work-tree=${HOME}'

alias dg='dotfiles_git'

grs () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git restore "$@"
    else
        git restore "$@"
    fi
}

grv () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git revert "$@"
    else
        git revert "$@"
    fi
}

grc () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git rm --cached "$@"
    else
        git rm --cached "$@"
    fi
}

gr () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git rm "$@"
    else
        git rm "$@"
    fi
}

gd () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git diff "$@"
    else
        git diff "$@"
    fi
}

ga () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git add "$@"
    else
        git add "$@"
    fi
}

gpl () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git pull "$@"
    else
        git pull "$@"
    fi
}

gp () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git push "$@"
    else
        git push "$@"
    fi
}

gsh () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git show "$@"
    else
        git show "$@"
    fi
}

gs () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git status "$@"
    else
        git status "$@"
    fi
}

gc () {
    if [[ "$PWD" = "$HOME" ]]; then
        dotfiles_git commit -m "$@"
    else
        git commit -m "$@"
    fi
}

#########
# IONIC #
#########

alias ionicbuild="ionic cordova build --release android"
alias keysignation="jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore ~/.github/ionicApp/my-key.keystore"

################
# DMENU COLORS #
################

[ -f "${HOME}/.cache/wal/colors.sh" ] && . "${HOME}/.cache/wal/colors.sh"

alias dmenu='/usr/local/bin/dmenu -nb "$color0" -nf "$color15" -sb "$color3" -sf "$color0"'

#######
# FZF #
#######

# Goto
g(){ [[ ! -n $1 ]] && search="/home/sam" || search=$1; cd "$(find $search -maxdepth 4 -not -path '*/\.*' -type d | fzf )" ;}
gh(){ [[ ! -n $1 ]] && search="/home/sam" || search=$1; cd "$(find $search -maxdepth 4 -type d | fzf )" ;}
# copy to
c(){ cp "$@" "$(find /home/sam -maxdepth 4 -type d | fzf)";}
# move to
m(){ mv "$@" "$(find /home/sam -maxdepth 4 -type d | fzf)";}
# search
f(){ [[ ! -n $1 ]] && search="/home/sam" || search=$1
     shit=$(find $search -type f -maxdepth 4 | fzf --layout=reverse --height 40% )
     if [[ -n $shit ]];then
     nohup xdg-open "$shit" &>/dev/null &
     echo "Opening $shit"
     sleep 1
     fi
   }

# edit
e(){ /bin/nohup /home/sam/.script/edit "$@" &>/dev/null &}

#######
# FEH #
#######

alias feh='feh -..'

# GRUB

alias grubreset='sudo grub-mkconfig -o /boot/grub/grub.cfg'

# MPD

alias mpddefault='killall mpd && cp ${HOME}/.config/mpd/configfiles/config_default.conf ${HOME}/.config/mpd/mpd.conf && mpd'
alias mpdstream='killall mpd && cp ${HOME}/.config/mpd/configfiles/config_stream.conf ${HOME}/.config/mpd/mpd.conf && mpd'

# AUR install

alias auwlr='${HOME}/.script/auwlr'

# git clone

clonegit () {
    repo="$1"
    [ -n "$2" ] && cd "$2"
    [ -n "$3" ] && user="$3"  || user="BrachystochroneSD"
    git clone --depth 1 "git@github.com:$user/$repo.git"
    cd "$repo"
}

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
alias bellwarn='paplay /usr/share/sounds/freedesktop/stereo/bell.oga'

# FFMPEG

mp42mov () {
    ffmpeg -i "$1" -c:v dnxhd -profile:v dnxhr_hq -pix_fmt yuv422p -c:a pcm_s16le -f mov "$(echo $1 | sed 's/.[^.]$//')".mov
}

flac2mp3 () {
    ffmpeg -i "$1" -ab 320k -map_metadata 0 -id3v2_version 3 "$2"
}

subextract () {
    ffmpeg -i "$1" -map 0:s:0 "${1%.*}.srt"
}

videocompress () {
    input_video=$1
    video_name=${input_video%%.*}
    video_ext=${input_video##*.}
    ffmpeg -i "$input_video" -vcodec libx265 -crf 28 "$video_name"_compressed."$video_ext"

}

# systemctl

alias sysus='systemctl --user'

# KTECH

texcheck () {
    ktech "$1" shitktech.png && feh shitktech.png && rm shitktech.png
}

myop() {
    nohup "$1" > /dev/null 2>&1 &
}
alias makesuck='rm config.h; make && sudo make install'

# xdg-mime

alias update-desktop-db='update-desktop-database ~/.local/share/applications'

alias xqf='xdg-mime query filetype'

xqd() {
    filetype=$(xdg-mime query filetype "$1")
    xdg-mime query default $filetype
}

xinfo() {
    file="$1"
    echo Filetype: $(xqf $1)
    echo Default application: $(xqd $1)
}

xdd() {
    application="$1"
    file="$2"
    filetype=$(xqf "$file")
    curr_default=$(xqd "$file")

    echo "Change default application of"
    echo "        $filetype "
    echo "     from $curr_default "
    echo "      to $application ? [y/N]"
    read yn
    [ "$yn" = "y" -o "$yn" = "Y" ] && xdg-mime default $application $filetype || echo aborted
}

# compress photo
compress_image () {
    [ -n "$2" ] && output="$2" || output="compress_$1"
    [ -f "$1" ] && ffmpeg -i "$1" -qscale:v 2 "$output" || echo "$1" not a file
}
