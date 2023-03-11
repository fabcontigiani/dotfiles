#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

HISTSIZE=20000
HITFILESIZE=20000

alias ls='exa -aF --icons  --group-directories-first'
alias ll='exa -lahF --icons --group-directories-first'
alias cat='bat --theme=gruvbox-dark'
alias vim='nvim'
alias pk='pikaur'
alias icat="kitty +kitten icat"
alias kdiff='git difftool --no-symlinks --dir-diff'

PS1='[\u@\h \W]\$ '
eval "$(starship init bash)"
