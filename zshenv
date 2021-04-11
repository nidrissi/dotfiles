#!/bin/zsh

export PATH="$HOME/.local/bin:$PATH"
export EDITOR='code --wait'

# tex
export PATH="$HOME/.texlive2021/bin/x86_64-linux:$PATH"
export MANPATH="$HOME/.texlive2021/texmf-dist/doc/man:$MANPATH"
export INFOPATH="$HOME/.texlive2021/texmf-dist/doc/info:$INFOPATH"

# nvm
export NVM_DIR="$HOME/.nvm"
export PATH="$HOME/.nvm/versions/node/v15.14.0/bin:$PATH" # hard-coded :-(
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use
