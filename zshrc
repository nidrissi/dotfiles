# -*- mode: sh; -*-
if [ "$TERM" = 'dumb' ]; then
    export PS1='$ '
    unsetopt zle
    return 0
fi

export ZSH="$HOME/.oh-my-zsh"
#ZSH_THEME="gentoo"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="%d/%m/%Y"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(cpanm)

# Must be before
eval $(dircolors $HOME/.dir_colors)

source $ZSH/oh-my-zsh.sh

# User configuration
zstyle ':completion:*' verbose yes

alias ll='ls -Ahl'
alias sctl=systemctl
alias jctl=journalctl


# change prompt color based on host
# __my_hash=0x$(hostname | md5sum | head -c 10)
# __my_color=$(($__my_hash % 6 + 2))    # from 2 to 7
case $HOST in
    'alpha.idrissi.eu')
        __my_color='yellow'
        ;;
    *)
        __my_color='blue'
        ;;
esac

# vcs info
autoload -Uz vcs_info
function precmd() {
    rehash
    vcs_info
}
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr '+'
zstyle ':vcs_info:*' unstagedstr '*'
zstyle ':vcs_info:git:*' formats '(%b%u%c) '
zstyle ':vcs_info:git:*' actionformats '(%b%u%c|%a) '

export PROMPT='%F{${__my_color}}%B%(!..%n@)%m%F{red} %~ %F{cyan}${vcs_info_msg_0_}%F{red}%#%f%b '
export PROMPT2='%F{${__my_color}}%B%(!..%n@)%m%F{red} %_>%f%b '

export RPROMPT='[%T]'

autoload -U zrecompile
zrecompile -p \
    -R ~/.zshrc -- \
    -M ~/.zcompdump
