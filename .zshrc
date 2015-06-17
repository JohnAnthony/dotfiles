source ~/.zshrc-grml
source ~/.profile

# The following lines were added by compinstall

#autoload -Uz compinit colors && colors
#compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
export GOPATH=~/go
#export GOROOT=~/go
export GOBIN=/usr/local/go/bin
setopt extendedglob appendhistory
#unsetopt beep nomatch notify
bindkey -v
# End of lines configured by zsh-newuser-install

export MSF_DATABASE_CONFIG=/home/john/metasploit-framework/database.yml

# My init stuff
#setopt noequals

# Emacs compatibility
[[ $EMACS = t ]] && unsetopt zle

# Customising the prompt
#typeset -a precmd_functions
#precmd_functions=""

# Common variables
export EDITOR="emacsclient"
export ALTERNATE_EDITOR="emacs"

# Aliases
alias irssi="dtach -A /tmp/irssi.dtach irssi"
alias newsbeuter="dtach -A /tmp/newsbeuter.dtach newsbeuter"
alias emacs="dtach -A /tmp/emacs.dtach emacs -nw"
alias proc='ps aux | grep -v grep | grep'
alias timestamp='date "+%s"'
alias emacs=emacssession

# Add extra paths
export PATH="/home/john/.cabal/bin:/usr/local/bin:$GOBIN:$PATH"
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
