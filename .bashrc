set -o vi
export TERM=rxvt-unicode
export PAGER="most"
export EDITOR="vim"
export CLASSPATH=$CLASSPATH:.
PATH=$PATH:/home/rohit/.bin:/opt/android-sdk/platform-tools:/home/rohit/bin
alias sudo='A=`alias` sudo '
alias pacman='pacman-color'
alias ls='ls --color=auto'
alias lock='gnome-screensaver-command --lock'
alias grep='grep --color=auto'
alias df='df -h'
alias du='du -c -h'
alias ping='ping -c 3'
alias mkdir='mkdir -p -v'
alias ..='cd ..'
alias rsync='rsync -a --stats --progress'
eval `dircolors -b`
#PS1='[\u@\h \W]\$ '
#PS1='\[\e[0;33m\]\u@\[\e[1;34m\]\h \W \[\e[1;37m\]\$ '
export GREP_COLOR="1;33"


