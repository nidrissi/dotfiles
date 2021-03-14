# dotfiles

This repository the various configuration files that I use for several pieces of software, notably [Emacs](https://www.gnu.org/software/emacs/), [zsh](https://www.zsh.org/), etc.
I mostly follow the XDG path specification.

- config
  - emacs: My Emacs configuration
    - init.el: small, loaded first
    - emacs.org: compiled by org-mode
    - custom.el: custom configuration that doesn't belong to packages
  - git
  - latexmk
- local
  - bin
    - arxiv.pl: old script
    - wslopen: open something with Windows' `start` command while inside WSL
    - yt-audio: uses youtube-dl to extract audio
  - share/applications
    - wslopen.desktop: in order to register wslopen as a possible application, use xdg-mime to set as default
- ssh/config
- dir_colors: used by zshrc
- wt.json: Windows Terminal (not really a dotfile)
- zshrc
