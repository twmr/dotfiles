# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

# User specific aliases and functions
source ~/.zsh/alias

LOCSOFT=$HOME/local/software
export NETGENDIR=$LOCSOFT/bin
export LD_LIBRARY_PATH=$LOCSOFT/lib/:$LOCSOFT/lib/Togl1.7:$LD_LIBRARY_PATH
export CFFEMBUILDDIR=$HOME/gitrepos/cf-fem-lib/build
export CFBD=$CFFEMBUILDDIR

export EPDPATH=$LOCSOFT/epd-7.2-1-rh5-x86_64/bin
export RANDPATH=$HOME/gitrepos/randomlas

export PATH=$RANDPATH:$CFFEMBUILDDIR/src:$EPDPATH:$NETGENDIR:$PATH

export LANG="C"
export LC_ALL="C"

#MPI stuff
export MYMPI_INC_PATH=/usr/mpi/qlogic/include
export MYMPI_LIB_PATH=/usr/mpi/qlogic/lib64

export MYSRCDIR=$HOME/local/src
export BOOST_SRC_PATH=$HOME/local/src/boost_1_47_0

#for the xml_pp program
export PERLLIB=/home/lv70072/thisch/bin/

function initcfbuild {
    LANG=C CC=mpicc CXX=mpicxx  cmake -DBOOST_ROOT=$BOOST_SRC_PATH \
        -DCMAKE_CXX_FLAGS="-I$MYSRCDIR" -DCMAKE_EXE_LINKER_FLAGS="-shared-intel" \
        -DCMAKE_BUILD_TYPE=Release -DNETGEN_SOURCE_DIR=$MYSRCDIR/netgen-4.9.13 \
        -DENABLE_MPI=ON \
        -DMPI_INCLUDE_PATH=$MYMPI_INC_PATH -DMPI_LIBRARY=$MYMPI_LIB_PATH \
        -DCMAKE_INSTALL_PREFIX=$LOCSOFT ..
    make -j8
}

function initnonmpicfbuild {
    LANG=C cmake -DBOOST_ROOT=$BOOST_SRC_PATH \
        -DCMAKE_CXX_FLAGS="-I$MYSRCDIR" \
        -DCMAKE_BUILD_TYPE=Release -DNETGEN_SOURCE_DIR=$MYSRCDIR/netgen-4.9.13 \
        -DCMAKE_INSTALL_PREFIX=$LOCSOFT ..
}

function makecfmpi {
    cd $CFBD; make -j8 pertubation; cd -
}

function makecfmpiall {
    cd $CFBD; make -j8 ; cd -
}

# function makecfmpi {
#     cd $CFBD; make -j8 pertubation; cd -
# }

alias gs='git status'
alias gd='git diff'
#alias python='python2.6'
#alias e='emacs -nw'
alias e='emacsclient -nw -a /usr/bin/emacs'
alias vim='emacsclient -nw -a /usr/bin/emacs'
alias started=${HOME}/gitrepos/dotfiles/emacs.d/start-emacs-server.sh
alias ked="e -e '(kill-emacs)'"
