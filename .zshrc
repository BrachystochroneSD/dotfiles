SAVEHIST=10
HISTFILE=~/.zsh_history


#color

autoload -U colors && colors

PS1="%B%{$fg[yellow]%}%n%{$fg[blue]%}@%{$fg[red]%}%M%{$fg[blue]%}:%{$fg[magenta]%}%~%{$reset_color%}$%b "

stty -ixon

autoload -U compinit
zstyle ':completion:*' menu select
compinit
_comp_options+=(globdots)

#aliases:

alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -alF'
alias l='ls -CF'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias diff='diff --color=auto'

#-----------------------------------------------#
#     _    _     ___    _    ____  _____ ____   #
#    / \  | |   |_ _|  / \  / ___|| ____/ ___|  #
#   / _ \ | |    | |  / _ \ \___ \|  _| \___ \  #
#  / ___ \| |___ | | / ___ \ ___) | |___ ___) | #
# /_/   \_\_____|___/_/   \_\____/|_____|____/  #
#                                               #
#-----------------------------------------------#

##################
# Basics Aliases #
##################

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias ls='ls --color=auto'
alias cl="clear"
alias ska="sudo killall"

alias xclipb='xclip -selection clipboard'

##########
# PACMAN #
##########

alias sp='sudo pacman'
alias sps='sp -S'
alias spsyyu='sp -Syyu'

#########
# EMACS #
#########

alias emacstty="~/.script/edit -t"
alias emacsconfig="~/.script/edit -t ~/.emacs"
e(){ /bin/nohup /home/sam/.script/edit2 "$@" &>/dev/null &}

############
# ZENOCYNE #
############

alias zenocloud="ssh pi@www.zenocyne.com"
alias zenocloudbis="ssh pi@192.168.0.102"

##################
# Custom scripts #
##################

alias twitch="~/.script/twitch/twitchscript"
alias gamedbupdate="~/.script/gamedatabase/gamedbupdate"
alias yts="~/.script/youtube/youtubesearch"

line_find(){ find "$1" -type f | grep "$2" | xargs cat | grep "$3";}

##########
# GITHUB #
##########

alias dotfiles_git='/bin/git --git-dir=${HOME}/.dotfiles --work-tree=${HOME}'
alias dotfiles_git_pom='/bin/git --git-dir=${HOME}/.dotfiles --work-tree=${HOME} push -u origin master'

alias dg='dotfiles_git'
alias dga='dotfiles_git add'
alias dgp='dotfiles_git_pom'
alias dgs='dotfiles_git status'

###########################
# Hdmi Port Double Screen #
###########################

alias hdmiScreen='xrandr --output HDMI1 --auto --left-of eDP1&& ~/.script/owl --owl'
alias hdmiScreenoff='xrandr --output HDMI1 --off'

#########
# IONIC #
#########

alias ionicbuild="ionic cordova build --release android"
alias keysignation="jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore ~/.github/ionicApp/my-key.keystore"

################
# DMENU COLORS #
################

. "${HOME}/.cache/wal/colors.sh"

alias dmenu='dmenu -nb "$color0" -nf "$color15" -sb "$color0" -sf "$color3"'

############
# APM SHIT #
############

alias apm_shit="sudo hdparm -B 254 /dev/sda"

####################
# ANDROID SDK PATH #
####################

PATH=$PATH:~/.sdk/android/tools

#######
# FZF #
#######

# GO TO
g(){ [[ ! -n $1 ]] && search="/home/sam" || search=$1; cd "$(find $search -not -path '*/\.*' -type d | fzf )" ;}
gh(){ [[ ! -n $1 ]] && search="/home/sam" || search=$1; cd "$(find $search -type d | fzf )" ;}
# copy to
c(){ cp "$@" "$(find /home/sam -type d | fzf)";}
#move to
m(){ mv "$@" "$(find /home/sam -type d | fzf)";}
# search
f(){ [[ ! -n $1 ]] && search="/home/sam" || search=$1
     shit=$(find $search -type f | fzf --layout=reverse --height 40% )
     if [[ -n $shit ]];then
	 nohup xdg-open "$shit" &>/dev/null &
	 echo "Opening $shit"
	 sleep 1
     fi
   }

alias cpr='cp -rv'
alias srczshrc='. /home/sam/.zshrc'

alias youtube2mp3='youtube-dl -x --embed-thumbnail --audio-format mp3'
alias duha='du -ha --max-depth=1'

alias mkpkg='makepkg -Acs'

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

#######
# FEH #
#######

alias feh='feh -..'
