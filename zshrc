#!/bin/zsh -*- mode: sh; -*-

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

## ignore some files for editors
binary=(.o .zo .zi .zix '.sw?' .jpg .gif .dvi .dvi.gz)
edit_ignore=(.aux .old .log '#' '~' $binary)
editors=(pico vim '*emacs' nedit nano joe mcedit cooledit)

zstyle ":completion:*:*:(${(j:*|:)editors}*):*" ignored-patterns \*${^edit_ignore}
zstyle ":completion:*:*:cat:*" ignored-patterns \*${^binary} '*.gz'

unset binary editors edit_ignore

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
alias sctl=systemctl
alias jctl=journalctl

# prompt
## change prompt color based on host
case $HOST in
    'xyz.idrissi.eu')
        __my_color='yellow'
        ;;
    'knot')
        __my_color='magenta'
        ;;
    *)
        __my_color='blue'
        ;;
esac

## vcs info
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr '+'
zstyle ':vcs_info:*' unstagedstr '*'
zstyle ':vcs_info:git:*' formats '(%b%u%c) '
zstyle ':vcs_info:git:*' actionformats '(%b%u%c|%a) '

setopt prompt_subst
export PROMPT='%F{${__my_color}}%B%(!..%n@)%m%F{red} %~ %F{cyan}${vcs_info_msg_0_}%F{red}%#%f%b '
export PROMPT2='%F{${__my_color}}%B%(!..%n@)%m%F{red} %_>%f%b '

export RPROMPT='[%T]'

# title & vcs info
function __my_title {
    case $TERM in
        xterm*|*rxvt*|screen)
            print -Pn "\e]2; $* \a"
            ;;
    esac
}
function precmd {
    __my_title "%~ %#"
    rehash
    vcs_info
}
function preexec {
    __my_title "%~ %# $2"
}

# compilation
autoload -U zrecompile
zrecompile -p \
    -R ~/.zshrc -- \
    -M ~/.zcompdump

# Windows shenanigans
if [[ $OS -eq "Windows_NT" ]]; then
    # Note: ~/.ssh/environment should not be used
    _env=~/.ssh/agent.env

    agent_is_running() {
        if [ "$SSH_AUTH_SOCK" ]; then
            # ssh-add returns:
            #   0 = agent running, has keys
            #   1 = agent running, no keys
            #   2 = agent not running
            ssh-add -l >/dev/null 2>&1 || [ $? -eq 1 ]
        else
            false
        fi
    }

    agent_has_keys() {
        ssh-add -l >/dev/null 2>&1
    }

    agent_load_env() {
        . "$_env" >/dev/null
    }

    agent_start() {
        (umask 077; ssh-agent >"$_env")
        . "$_env" >/dev/null
    }

    if ! agent_is_running; then
        agent_load_env
    fi

    # if your keys are not stored in ~/.ssh/id_rsa or ~/.ssh/id_dsa, you'll need
    # to paste the proper path after ssh-add
    if ! agent_is_running; then
        agent_start
        ssh-add
    elif ! agent_has_keys; then
        ssh-add
    fi

    unset _env
fi

# local configuration
if [[ -e "$HOME/.zshrc.local" ]]; then
    . $HOME/.zshrc.local
fi

# syntax highlighting
# must be last
source $HOME/.zplugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
