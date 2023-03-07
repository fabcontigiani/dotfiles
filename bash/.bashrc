#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='exa -a --group-directories-first'
alias ll='exa -lah --group-directories-first'
alias vim='nvim'

PS1='[\u@\h \W]\$ '
eval "$(starship init bash)"

