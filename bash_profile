# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:/opt/ghc/bin:$HOME/bin
OMP_NUM_THREADS=1
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/python-init/lib/
export PATH
export OMP_NUM_THREADS

echo my zsh vers $ZSH_VERSION
[ x$ZSH_VERSION = x -a -f $HOME/local/software/bin/zsh ] && exec $HOME/local/software/bin/zsh -l
