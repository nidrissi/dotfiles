#!/bin/zsh
# .zshrc

# Init
## Tramp (Emacs)
if [ "$TERM" = "dumb" ] ; then
    export PS1='$ '
    unsetopt zle
    return 0
fi


# Completion configuration
## Pretty colors
eval $(dircolors -b ~/.dir_colors)
autoload -U colors; colors

## Completers list
zstyle ':completion:*' completer _expand _complete _ignored

## Formatting & Messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''

## Insert all possibilites for _expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

## Completion for processes
[ "$USER" = "root" ] && SWITCH='-A' || SWITCH="-u ${USER}"
zstyle ':completion:*:processes*' menu yes select
zstyle ':completion:*:processes-names' command \
    "ps c $SWITCH -o command | uniq"
zstyle ':completion:*:processes' command \
    "ps c $SWITCH -o pid -o command | uniq"
unset SWITCH

## Color in ls completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

## Ignore *~ files and _* functions
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?~'
zstyle ':completion:*:rm:*' ignore-line yes

## Ignore some files for editors
binary=(.o .zo .zi .zix '.sw?' .jpg .gif .dvi .dvi.gz)
edit_ignore=(.aux .old .log '#' '~' $binary)
editors=(pico vim '*emacs' nedit nano joe mcedit cooledit)

zstyle ":completion:*:*:(${(j:*|:)editors}*):*" ignored-patterns \
\*${^edit_ignore}
zstyle ":completion:*:*:cat:*" ignored-patterns \*${^binary} '*.gz'

unset binary editors edit_ignore

## Install it, load it
zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit


# Options

## History
HISTFILE=~/.history
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


# Misc

## Some nice key bindings
bindkey -e
bindkey ' ' magic-space       # also do history expansion on space

## Prompt
PROMPT="%{$fg[green]%}%n@%M %B%{$fg[magenta]%}%(4~,./%2~,%~) %{$fg[red]%}%# %f%b"
PROMPT2="%B%{$fg[magenta]%}%_ %{$fg[red]%}> %f%b"
RPROMPT="%{$fg[cyan]%}%T%{$fg[white]%}%b"
export PROMPT PROMPT2

## Title
function title {
    case $TERM in
        xterm*|*rxvt*|screen)
            print -Pn "\e]2; $* \a"
            ;;
    esac
}
function precmd { title "%~ %#"; rehash }
function preexec { title "%~ %# $2" }

## Aliases
alias j='jobs -l'
alias d='dirs -v'
alias p=popd
### Colors
alias ls='ls --color=auto -F'
alias ll='ls -Ahl'

## ESC h to run help
alias run-help >&/dev/null && unalias run-help
autoload run-help

# Recompile
autoload -U zrecompile
zrecompile -p \
    -R ~/.zshrc -- \
    -M ~/.zcompdump
[ $SHLVL -eq 1 ] && eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"
