# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

LS_COLORS='*=1;37:di=1;31:ex=0;34:*.exe=0;34:*.com=0;34:*.wmv=1;35:*.avi=1;35:*.flv=1;35:*.webm=1;35:*.mkv=1;35:*.mpg=1;35:*.mpeg=1;35:*.ogv=1;35:*.jpg=1;33:*.jpeg=1;33:*.png=1;33:*.gif=1;33:*.bmp=1;33:*.rar=0;32:*.zip=0;32:*.tar.gz=0;32:*.tar.bz=0;32:*.tar.gz*=0;32:*.tar.bz*=0;32:*.pdf=0;36:*.chm=0;36:*.pl=0;34:*.py=0;34:*.cpp=0;34:*.c=0;34:*.hpp=0;34:*.h=0;34:*.hs=0;34:*.css=0;34:*.php=0;34:*.html=0;34:*.htm=0;34:*.css=0;34:*.js=0;34:*.hta=0;34:*.cbr=0;36:*.cbz=0;36:*.mp3=1;35:*.ogg=1;35:*.wav=1;35:*.mp4=1;35:*.7z=0;32'

alias s=sudo
alias ls="ls --color -F"
alias z=zathura
alias stamp="date +%s-%N"
alias gdb "gdb -q"
alias sprunge='curl -F "sprunge=<-" http://sprunge.us'
alias pb='qiv -muw 1920'
alias pbshuf='qiv -Smuw 1920'
alias mfs="mplayer -fs -shuffle"
alias trr=transmission-remote
alias trd=transmission-daemon

alias sa="sudo aptitude"
alias a=aptitude
alias sp="sudp pacman"
alias p=pacman

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
