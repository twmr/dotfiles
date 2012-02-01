#!/bin/zsh
#
# Dircolors...
eval `dircolors -b ~/.dircolors`

# Kill flow control
if tty -s ; then
    stty -ixon
    stty -ixoff
fi

#export LC_ALL=en_US.UTF-8
#export LANG=en_US.UTF-8
#export LOCALE=en_US.UTF-8
export BROWSER=chromium-browser
export OOO_FORCE_DESKTOP='gnome'
export EDITOR="emacsclient -c -a /usr/bin/emacs -nw"
export VISUAL="$EDITOR"
export HISTCONTROL=ignoredups
export IGNOREEOF=3
export AWT_TOOLKIT=MToolkit # for matlab
HOSTNAME=`hostname`

# Exports
export PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin

if [ "$HOSTNAME" = "firebird" ]; then
    arch="ia32"
    intel_version="11.1/069"
    intel_prefix="/opt/intel/Compiler/"

    export PATH=$PATH:/opt/maple10/bin:$HOME/local/Moves2/bin
    export MATLAB_JAVA=/usr/lib/jvm/java-1.6.0-openjdk/jre
    export CVSROOT=:pserver:hisch@localhost:/usr/local/cvsroot/quest.root
    export SESSA_DATABASE_PATH=$HOME/CVSrepos/Development/databases
    export CFFEM_MLCODE=/home/thomas/cf-fem-lib/matlab

elif [ "$HOSTNAME" = "mustang" ]; then
    arch="intel64"
    intel_version="11.1/046"
    intel_prefix="/opt/intel/Compiler/"

    export MYSRCDIR=$HOME/local/src
    export LOCSOFT=$HOME/local/software

    export MATLAB_BIN=/usr/local/MATLAB/R2010b/bin
    export MATLAB_JAVA=/usr/lib/jvm/java-1.6.0-openjdk-1.6.0.0.x86_64/jre

    export CFFEM_REPO=${HOME}/cf-fem-lib
    export CFFEM_MLCODE=${CFFEM_REPO}/matlab
    export CFBD=${CFFEM_REPO}/build
    export RANDOMLAS=${HOME}/gitrepos/randomlas

    #cmake -DCMAKE_CXX_FLAGS=-I{$MYSRCDIR} -DCMAKE_BUILD_TYPE=Debug -DNETGEN_SOURCE_DIR=${NETGENSRC}
    #-DCMAKE_INSTALL_PREFIX=${LOCSOFT} -DENABLE_MPI=ON ..

    export NETGENDIR=$LOCSOFT/bin
    export NETGENSRC=$MYSRCDIR/netgen-4.9.13 #for cffemlib compilation
    NGSOLVE_PATH=${MYSRCDIR}/ngsolve-4.9.13
    TOGL_PATH=${LOCSOFT}/lib/Togl1.7

    #sessa
    export CVSROOT=:pserver:hisch@localhost:/usr/local/cvsroot/quest.root
    export SESSA_DATABASE_PATH=/home/thomas/gitrepos/sessa-git/databases

    #parallel stuff (mpi + petsc + slepc)

    export MYMPI_INC_PATH=/usr/include/openmpi-x86_64
    export MYMPI_LIB_PATH=/usr/lib64/openmpi/lib
    export MYMPI_BIN_PATH=/usr/lib64/openmpi/bin


    export PETSC_DIR=${MYSRCDIR}/petsc-3.2-p6
    export PETSC_ARCH="arch-linux2-cxx-debug"
    #./configure --with-c++-support=1 --with-scalar-type=complex
    #--with-x11=0 --with-clanguage=cxx
    #--with-blas-lapack-dir=/opt/intel/Compiler/11.1/046/mkl/lib
    #CXXOPTFLAGS="-O3" COPTFLAGS="-O3" FOPTFLAGS="-03"

    #./configure --with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-clanguage=cxx --with-blas-lapack-dir=/opt/intel/Compiler/11.1/046/mkl/lib CXXOPTFLAGS="-O3" COPTFLAGS="-O3" FOPTFLAGS="-03" --with-shared-libraries=1


    export SLEPC_DIR=${MYSRCDIR}/slepc-3.2-p3
    #./configure  #suffices
    export LD_LIBRARY_PATH=${MYMPI_LIB_PATH}:${LOCSOFT}/lib:${TOGL_PATH} #:${LD_LIBRARY_PATH}
    export PATH=${LOCSOFT}/bin:${HOME}/gitrepos/emacs/src:${MYMPI_BIN_PATH}:${MATLAB_BIN}:${PATH}

elif [ "$HOSTNAME" = "thisch" ]; then
    #arch="intel64"
    intel_arch="intel64"
    intel_version=""
    intel_prefix="/home/thomas/intel"

    export MYSRCDIR=${HOME}/local/src
    export LOCSOFT=$HOME/local/software

    export MATLAB_BIN=${HOME}/MATLAB/R2010b/bin
    export MATLAB_JAVA=/usr/lib/jvm/java-1.6.0-openjdk/jre

    export CFFEM_REPO=${MYSRCDIR}/cf-fem-lib
    export CFFEM_MLCODE=${CFFEM_REPO}/matlab
    export CFBD=${CFFEM_REPO}/build
    export RANDOMLAS=${CFFEM_REPO}/examples/2DFEM/randomlas

    #TODO compile instructions cf-fem-lib

    export NETGENDIR=/usr/local/bin #netgen needs this envvar
    export NETGEN_SRC_PATH=${MYSRCDIR}/netgen-4.9.13
    export NGSOLVE_PATH=${MYSRCDIR}/ngsolve_with_mkl/installed
    export NGSOLVE_SRC_PATH=${MYSRCDIR}/ngsolve_with_mkl
    TOGL_PATH=${MYSRCDIR}/Togl-1.7

    #TODO upgrade boost
    export BOOST_PATH=${MYSRCDIR}/boost_1_46_1

    . ${intel_prefix}${intel_version}/bin/compilervars.sh ${intel_arch}

    export LD_LIBRARY_PATH=${TOGL_PATH}:/usr/local/lib:${LD_LIBRARY_PATH}
    export PATH=$HOME/qtcreator-2.4.0/bin/:${MATLAB_BIN}:$PATH

    #parallel stuff (mpi + petsc + slepc )

    export LD_LIBRARY_PATH=/usr/lib64/openmpi/lib:$LD_LIBRARY_PATH

    export PETSC_DIR=${MYSRCDIR}/petsc-3.2-p6
    export PETSC_ARCH=arch-linux2-cxx-release
    #./configure --with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-clanguage=cxx --with-blas-lapack-dir=~/intel/mkl/lib/intel64 CXXOPTFLAGS="-O3" COPTFLAGS="-O3" FOPTFLAGS="-03" --with-shared-libraries=1 --with-debugging=0

    export SLEPC_DIR=${MYSRCDIR}/slepc-3.2-p3
    #./configure  #suffices

elif [ "$HOSTNAME" = "l01" ]; then
    arch=""

    echo TODO

else
    arch=""
fi

#if [[ "$arch" = "ia32" ]] || [[ "$arch" = "intel64" ]]; then
if [ "$arch" ]; then
    . ${intel_prefix}${intel_version}/bin/$arch/iccvars_$arch.sh
    . ${intel_prefix}${intel_version}/bin/$arch/ifortvars_$arch.sh
fi

if [ "$HOSTNAME" = "thisch" -o "$HOSTNAME" = "l01" -o "$HOSTNAME" = mustang ]; then
    if [ -z "${RANDOMLAS}" ]; then
        echo Warning RANDOMLAS not defined
    else
        hash -d rand=${RANDOMLAS}
    fi
    if [ -z "${CFFEM_REPO}" ]; then
        echo Warning CFFEM_REPO not defined
    else
        hash -d cfrepo=${CFFEM_REPO}
    fi


    #cffemlib + simulation stuff
    export PYTHONPATH=${CFFEM_REPO}/tools/in2d_creator_scripts:${RANDOMLAS}/scripts:${RANDOMLAS}:${LOCSOFT}/nlopt/lib/python2.7/site-packages

    export PATH=${RANDOMLAS}:${RANDOMLAS}/scripts:${CFBD}/green:${CFBD}/src:${PATH}

    #todo test togl variable stuff
    if [ -z "${NGSOLVE_PATH}" ]; then
        echo Warning NGSOLVE_PATH not defined
    fi
    export LD_LIBRARY_PATH=${CFBD}/lib:${NGSOLVE_PATH}/lib:${LD_LIBRARY_PATH}

fi
# watch for people
#watch=(notme)                   # watch for everybody but me
#LOGCHECK=300                    # check every 5 min for login/logout activity

. ~/.zsh/linuxconsole
