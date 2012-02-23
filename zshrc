#!/bin/zsh

ONVSC=`hostname | egrep '(l01|r[0-9]+{2}n[0-9]+{2})'`
if [ "$ONVSC" ]; then
  echo LOADING zshrc
  fpath=${LOCSOFT}/share/zsh/4.3.15/functions
fi

. ~/.zsh/keybindings
. ~/.zsh/function
. ~/.zsh/style
. ~/.zsh/opts
. ~/.zsh/alias
. ~/.zsh/prompt
