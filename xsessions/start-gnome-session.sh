#!/bin/dash
# personal gnome-session script (for gnome-session version 2.32)
# by thomas hisch <thomas@opentech.at>

# if something with the gnome-session does go wrong look at .xsession-errors in your homedir

MYWM=$1
SHOW_DESKTOP=$2
GNOME3=true
HOSTNAME=`hostname`

SESSKEY="/desktop/gnome/session"
GSETTOOL="/usr/bin/gconftool-2"

# Load resources
if [ -f $HOME/.Xresourses ]; then
  xrdb -merge $HOME/.Xresources
fi

# TODO: when is it better to start apps here and when is it better to
# put the apps in .config/autostart and start it using a session
# manager (gnome-session)

# Set the background color
#/usr/bin/xsetroot -solid black
pgrep -x -u $USER urxvtd > /dev/null || urxvtd -o -f &

#The following line is now in gnome-session
pgrep -u $USER emacs > /dev/null || emacs --daemon &

#international keyboard layout
setxkbmap us altgr-intl
# xmodmap ~/.Xmodmap

setxkbmap -option ctrl:swapcaps     # Swap Left Control and Caps Lock
# setxkbmap -option ctrl:nocaps       # Make Caps Lock a Control key

~/gitrepos/dotfiles/bin/dualhead-ims.sh

/usr/bin/gnome-keyring-daemon --start --components=pkcs11
/usr/bin/gnome-keyring-daemon --start --components=ssh
/usr/bin/gnome-keyring-daemon --start --components=gpg

$GSETTOOL -u $SESSKEY/required_components/windowmanager

#if something does not work as expected start gnome-session with --debug and look into .xsession-errors
#-a starts only the *.desktop files in the specified dir instead of the WM  (ex: -a /home/thomas/dumm)
#exec strace -fF -o /tmp/gnome-session-trace /usr/bin/gnome-session

gpg-agent --daemon --enable-ssh-support \
          --write-env-file "${HOME}/.gpg-agent-info"

if [ -f "${HOME}/.gpg-agent-info" ]; then
   . "${HOME}/.gpg-agent-info"
   export GPG_AGENT_INFO
   export SSH_AUTH_SOCK
fi

exec /usr/bin/awesome

#rm -f ~/.config/gnome-session/saved-session/gnome-shell.desktop
#exec gnome-session --debug --session=awesome
