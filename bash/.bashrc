#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='exa -aF --icons  --group-directories-first'
alias ll='exa -lahF --icons --group-directories-first'
alias cat='bat --theme=gruvbox-dark'
alias vim='nvim'

PS1='[\u@\h \W]\$ '
eval "$(starship init bash)"

