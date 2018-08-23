# -*- mode: sh; -*-
export ZSH="/home/najib/.oh-my-zsh"
ZSH_THEME="agnoster"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

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
plugins=(cpanm git ssh-agent zsh-dircolors-solarized)

# Must be before
eval $(dircolors $HOME/.dir_colors)

source $ZSH/oh-my-zsh.sh

# User configuration
export EDITOR='emacs'
alias ll='ls -Ahl'
zstyle ':completion:*' verbose yes
[ $SHLVL -eq 1 ] && eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"

# rehash every time
function precmd() { rehash }

autoload -U zrecompile
zrecompile -p \
    -R ~/.zshrc -- \
    -M ~/.zcompdump
