# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
   . ~/.bashrc
fi

# User specific environment and startup programs
export PATH=$HOME/bin:$PATH
export OMP_NUM_THREADS=1
#LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/python-init/lib/

ONVSCMAIN=`hostname | egrep '(l01)' `
if [ "$ONVSCMAIN" ]; then
    echo "start zsh"
    initzsh
fi
