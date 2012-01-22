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
export PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/bin

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

    export PYTHONPATH=$CFFEM_REPO/tools/in2d_creator_scripts:${RANDOMLAS}/scripts:${RANDOMLAS}
    #cmake -DCMAKE_CXX_FLAGS=-I{$MYSRCDIR} -DCMAKE_BUILD_TYPE=Debug -DNETGEN_SOURCE_DIR=${NETGENSRC}
    #-DCMAKE_INSTALL_PREFIX=${LOCSOFT} -DENABLE_MPI=ON ..

    export NETGENDIR=$LOCSOFT/bin
    export NETGENSRC=$MYSRCDIR/netgen-4.9.13 #for cffemlib compilation

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

    export LD_LIBRARY_PATH=${MYMPI_LIB_PATH}:$LOCSOFT/lib:$LOBSOFT/lib/Togl1.7/:$LD_LIBRARY_PATH
    export PATH=$LOCSOFT/bin:$HOME/gitrepos/emacs/src:${MYMPI_BIN_PATH}:${MATLAB_BIN}:$PATH

elif [ "$HOSTNAME" = "thisch" ]; then
    #for DA
    export CFFEM_REPO=/home/thomas/cf-fem-lib
    export CFFEM_MLCODE=${CFFEM_REPO}/matlab
    export CFBD=${CFFEM_REPO}/build
    export RANDOMLAS=${CFFEM_REPO}/examples/2DFEM/randomlas
    export PYTHONPATH=$CFFEM_REPO/tools/in2d_creator_scripts:${RANDOMLAS}/scripts:${RANDOMLAS}

    PACK_PATH=${HOME}/packages
    export NETGENDIR=/usr/local/bin
    NGSOLVE_PATH=${PACK_PATH}/ngsolve_with_mkl/installed
    export BOOST_PATH=${PACK_PATH}/boost_1_46_1

    #arch="intel64"
    intel_arch="intel64"
    intel_version=""
    intel_prefix="/home/thomas/intel"
    . ${intel_prefix}${intel_version}/bin/compilervars.sh ${intel_arch}

    export MATLAB_JAVA=/usr/lib/jvm/java-1.6.0-openjdk/jre

    export LD_LIBRARY_PATH=${CFBD}/lib:${NGSOLVE_PATH}/lib:${PACK_PATH}/Togl-1.7:/usr/local/lib:$LD_LIBRARY_PATH
    export PATH=$RANDOMLAS:$CFBD/green:$CFBD/src:$HOME/qtcreator-2.4.0/bin/:$HOME/MATLAB/R2010b/bin:$PATH

    #parallel stuff (mpi + petsc + slepc )

    export LD_LIBRARY_PATH=/usr/lib64/openmpi/lib:$LD_LIBRARY_PATH

    export PETSC_DIR=${PACK_PATH}/petsc-3.2-p6
    export PETSC_ARCH=arch-linux2-cxx-debug
    #./configure --with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-clanguage=cxx --with-blas-lapack-dir=/opt/intel/Compiler/11.1/046/mkl/lib CXXOPTFLAGS="-O3" COPTFLAGS="-O3" FOPTFLAGS="-03" --with-shared-libraries=1

    export SLEPC_DIR=${PACK_PATH}/slepc-3.2-p3
    #./configure  #suffices

else
    arch=""
fi

#if [[ "$arch" = "ia32" ]] || [[ "$arch" = "intel64" ]]; then
if [ "$arch" != "" ]; then
    . ${intel_prefix}${intel_version}/bin/$arch/iccvars_$arch.sh
    . ${intel_prefix}${intel_version}/bin/$arch/ifortvars_$arch.sh
fi

# watch for people
#watch=(notme)                   # watch for everybody but me
#LOGCHECK=300                    # check every 5 min for login/logout activity

. ~/.zsh/linuxconsole
