# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
source ~/.zsh/alias

export NETGENDIR=$HOME/local/software/bin
export LD_LIBRARY_PATH=/home/lv70072/thisch/local/software/lib/:$HOME/local/software/lib/Togl1.7:$LD_LIBRARY_PATH
export PATH=/home/lv70072/thisch/local/software/epd-7.2-1-rh5-x86_64/bin:$NETGENDIR:$PATH

#for the xml_pp program
export PERLLIB=/home/lv70072/thisch/bin/


alias gs='git status'
alias gd='git diff'
#alias python='python2.6'
#alias e='emacs -nw'
alias e='emacsclient -nw -a /usr/bin/emacs'
alias vim='emacsclient -nw -a /usr/bin/emacs'
alias started=${HOME}/gitrepos/dotfiles/emacs.d/start-emacs-server.sh
alias ked="e -e '(kill-emacs)'"
