#!/bin/zsh

if [ "$TERM" = 'dumb' ]; then
    export PS1='$ '
    unsetopt zle
    return 0
fi

# completion
## colors
eval $(dircolors -b ~/.dir_colors)
autoload -U colors; colors

## completers list
zstyle ':completion:*' completer _expand _complete _ignored

## formatting & messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''

## insert all possibilites for _expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

## processes
[ "$USER" = "root" ] && SWITCH='-A' || SWITCH="-u ${USER}"
zstyle ':completion:*:processes*' menu yes select
zstyle ':completion:*:processes-names' command \
    "ps c $SWITCH -o command | uniq"
zstyle ':completion:*:processes' command \
    "ps c $SWITCH -o pid -o command | uniq"
unset SWITCH

## color in ls completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

## ignore *~ files and _* functions
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?~'
zstyle ':completion:*:rm:*' ignore-line yes

## Install it, load it
zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit

# Options
## History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=${HISTSIZE}
setopt \
    share_history \
    hist_ignore_dups \
    hist_verify \
    hist_no_store \
    hist_ignore_space \
    extended_history

## Others
DIRSTACKSIZE=20
setopt \
    NO_auto_menu \
    extended_glob \
    auto_pushd \
    interactive_comments \
    print_exit_value \
    auto_cd

## Some nice key bindings
bindkey -e
bindkey ' ' magic-space       # also do history expansion on space

## aliases
alias ls='ls --color'
alias ll='ls -Ahl'

# prompt
## change prompt color based on host
case $HOST in
    'ring')
        __my_color='yellow'
        ;;
    'knot')
        __my_color='#FFA500'
        ;;
    'tqft')
        __my_color='green'
        ;;
    'framboise')
        __my_color='#C41949';
        ;;
    'diskoid')
        __my_color='magenta'
        ;;
    *)
        __my_color='blue'
        ;;
esac

setopt prompt_subst
export PROMPT='%F{${__my_color}}%B%(!..%n@)%m%F{red} %~ %#%f%b '
export PROMPT2='%F{${__my_color}}%B%(!..%n@)%m%F{red} %_>%f%b '

# function __my_title {
#     case $TERM in
#         xterm*|*rxvt*|screen)
#             print -Pn "\e]2; $* \a"
#             ;;
#     esac
# }
# function precmd {
#     __my_title "%~ %#"
#     rehash
# }
# function preexec {
#     __my_title "%~ %# $2"
# }

# WSL-specific
## vcxsrv
# function xsetup() {
#     export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0.0
#     export NO_AT_BRIDGE=1           # https://bbs.archlinux.org/viewtopic.php?id=228894
#     export LIBGL_ALWAYS_INDIRECT=1
#     setxkbmap 'fr(oss)' &!
# }

## env
export PATH="$HOME/.local/bin:$PATH"
export EDITOR='code --wait'
### tex
export PATH="$HOME/.texlive2021/bin/x86_64-linux:$PATH"
export MANPATH="$HOME/.texlive2021/texmf-dist/doc/man:$MANPATH"
export INFOPATH="$HOME/.texlive2021/texmf-dist/doc/info:$INFOPATH"
### nvm
export NVM_DIR="$HOME/.nvm"
export PATH="$HOME/.nvm/versions/node/v15.13.0/bin:$PATH" # hard-coded :-(
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use

# compilation
autoload -U zrecompile
zrecompile -p \
           -R ~/.zshrc -- \
           -M ~/.zcompdump
