#/bin/bash

date >> ~/.emacs.d/emacs-server.log
pgrep -u $USER emacs > /dev/null || emacs --daemon >> ~/.emacs.d/emacs-server.log 2>&1

