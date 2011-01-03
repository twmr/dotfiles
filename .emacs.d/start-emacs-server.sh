#/bin/dash

date >> ~/.emacs.d/emacs-server.log
pgrep -u `whoami` emacs > /dev/null || emacs --daemon &>> ~/.emacs.d/emacs-server.log

