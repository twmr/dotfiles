#!/bin/bash
# initial version of this script by Who-T

pwd=$PWD
filename=`basename $0`
excludes=".gitignore .gitmodules laptop-settings.sh $filename xsessions/ gentoo xinitrc.opensuse"
confdirs="awesome gnome-terminal"

hostname=`hostname`

if test ${hostname} == "thisch"; then
    excludes = "${exlcudes} vpnc/"
fi

for file in `git ls-files | sed 's/\/.*/\//' | uniq`; do
    skip=0

    for exclude in $excludes; do
        if test $file = $exclude; then
            skip=1
            break
        fi
    done
    if test $skip -eq 1; then
        continue
    fi

    if test $(echo $file | cut -d. -f1) = "gtk-bookmarks"; then
        if test $(echo $file | cut -d. -f2) != $HOSTNAME; then
            continue
        fi
        file="gtk-bookmarks" #strip the hostname
    fi

    #strip last slash (directories) from filename
    file=${file%/}

    skip=0
    for cdir in $confdirs; do
        if test $file = $cdir; then
            if ! test -e "$HOME/.config/$file"; then
                echo ln -s "$pwd/$file" "$HOME/.$file"
                ln -s "$pwd/$file" "$HOME/.$file"
                #todo test if it points to correct directory/file
            elif ! test -L "$HOME/.config/$file"; then
                echo "$HOME/.config/$file already exists but is not a symbolic link - don't know what to do"
            else
                echo "$HOME/.config/$file already exists"
                #todo Overwrite it [y]n ....
            fi
            skip=1
            break
        fi
    done
    if test $skip -eq 1; then
        continue
    fi

    if ! test -e "$HOME/.$file"; then
        if test $file = "gtk-bookmarks"; then
            echo ln -s "$pwd/$file.$HOSTNAME" "$HOME/.$file"
            ln -s "$pwd/$file.$HOSTNAME" "$HOME/.$file"
        else
            echo ln -s "$pwd/$file" "$HOME/.$file"
            ln -s "$pwd/$file" "$HOME/.$file"
        fi
        #todo test if it points to correct directory/file
    elif ! test -L "$HOME/.$file"; then
        echo "$HOME/.$file already exists but is not a symbolic link - don't know what to do"
    else
        echo "$HOME/.$file already exists"
        #todo Overwrite it [y]n ....
    fi
done
