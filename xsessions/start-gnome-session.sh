#!/bin/dash
# personal gnome-session script (for gnome-session version 2.32)
# by thomas hisch <thomas@opentech.at>

# if something with the gnome-session does go wrong look at .xsession-errors in your homedir

MYWM=$1
SHOW_DESKTOP=$2
GNOME3=false

HOSTNAME=`hostname`
if [ "$HOSTNAME" = "thisch" ]; then
    GNOME3=true
fi

SESSKEY="/desktop/gnome/session"
GSETTOOL="/usr/bin/gconftool-2"

# Load resources
if [ -f $HOME/.Xresourses ]; then
  xrdb -merge $HOME/.Xresources
fi
if [ -f $HOME/.Xdefaults ]; then
  xrdb -merge $HOME/.Xdefaults
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
xmodmap ~/.Xmodmap


# The following is a simple hack to avoid starting
# gnome-panel/metacity when awesome was supposed to be started. this
# should be fixed in gnome-session (bug already filed under
# https://bugzilla.gnome.org/show_bug.cgi?id=638677 )
if [ "$MYWM" = "awesome" -a "$GNOME3" != "true" ]
then
    desktopfiles=`find ~/.config/gnome-session/saved-session -name "*.desktop"`
    if [ "x$desktopfiles" != "x" ] 
    then
        matchedfiles=`grep -Eil '(metacity|gnome-panel)' $desktopfiles`
        rm -f $matchedfiles
    fi
fi


# Update the Gnome configuration to reflect WM and desktop appearance
# IMPORTANT: $MYWM should have a valid .desktop file in
# /usr/share/applications or in .local/share/applications (for other
# dirs see log of `gnome-session --debug`) otherwise gsm will complain
# about it

$GSETTOOL -t boolean -s /apps/nautilus/preferences/show_desktop  ${SHOW_DESKTOP}

if [ "$GNOME3" = "true" ]
then
    $GSETTOOL -u $SESSKEY/required_components/windowmanager
else
    $GSETTOOL -s $SESSKEY/required_components/windowmanager ${MYWM} -t string

    if [ "$MYWM" = "awesome" ]
    then
        $GSETTOOL -s -t list --list-type string $SESSKEY/required_components_list "[windowmanager]"
        $GSETTOOL -s -t list --list-type string $SESSKEY/default_session "[gnome-settings-daemon]"
    else
        $GSETTOOL -s -t list --list-type string $SESSKEY/required_components_list "[filemanager,panel,windowmanager]"
        $GSETTOOL -s -t list --list-type string $SESSKEY/default_session "[gnome-settings-daemon]"
    fi
fi


#if something does not work as expected start gnome-session with --debug and look into .xsession-errors
#-a starts only the *.desktop files in the specified dir instead of the WM  (ex: -a /home/thomas/dumm)
#exec strace -fF -o /tmp/gnome-session-trace /usr/bin/gnome-session
#exec /usr/bin/awesome

#echo TESTVARS
#echo $GNOME3
#echo $MYWM
if [ "$GNOME3" = "true" -a "$MYWM" = "awesome" ]
then
    exec gnome-session --session=awesome
else
    export WINDOW_MANAGER=/usr/bin/$1
    exec gnome-session
fi

