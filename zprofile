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
export OOO_FORCE_DESKTOP='gnome'
export EDITOR="emacs" #is an alias to emacsclient
export VISUAL="$EDITOR"
export PAGER="vless"
export HISTCONTROL=ignoredups
export IGNOREEOF=3
# export AWT_TOOLKIT=MToolkit # for matlab
HOSTNAME=`hostname`

# see http://blog.vanutsteen.nl/2013/04/30/using-your-desktop-s-git-author-everywhere-you-ssh/
export GIT_AUTHOR_NAME="`git config user.name`"
export GIT_AUTHOR_EMAIL="`git config user.email`"
export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME
export GIT_COMMITTER_EMAIL=$GIT_AUTHOR_EMAIL

# Exports
export PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin
export MYSRCDIR=$HOME/local/src
export LOCSOFT=$HOME/local/software
export GITR=$HOME/gitrepos
export BROWSER=google-chrome

prepath() {
    # prepend path to PATH
    [ -d "${1}" ] && [[ ! "${PATH}" =~ ${1} ]] && PATH="${1}:${PATH}"
}

preldlpath() {
    # prepend path to LD_LIBRARY_PATH
    [ -d "${1}" ] && [[ ! "${LD_LIBRARY_PATH}" =~ ${1} ]] && LD_LIBRARY_PATH="${1}:${LD_LIBRARY_PATH}"
}

if [ "$HOSTNAME" = "mustang" ]; then
    arch="" #intel64"
    #intel_version="11.1/046"
    #intel_prefix="/opt/intel/Compiler/"

    export MATLAB_BIN=/usr/local/MATLAB/R2010b/bin
    export MATLAB_JAVA=/usr/lib/jvm/java-1.6.0-openjdk-1.6.0.0.x86_64/jre

    export PYCHARM_JDK=$MATLAB_JAVA

    export CFFEM_REPO=${HOME}/cf-fem-lib
    export CFFEM_MLCODE=${CFFEM_REPO}/matlab
    export CFBD=${CFFEM_REPO}/build
    export RANDOMLAS=${HOME}/gitrepos/randomlas
    export LAMBDAFOUR=${CFFEM_REPO}/examples/1DFDM/lambda4tests

    #CFFEM COMPILE
    #cmake -DCMAKE_CXX_FLAGS=-I{$MYSRCDIR} -DCMAKE_BUILD_TYPE=Debug -DNETGEN_SOURCE_DIR=${NETGENSRC}
    #-DCMAKE_INSTALL_PREFIX=${LOCSOFT} -DENABLE_MPI=ON ..

    export NETGENDIR=$LOCSOFT/bin
    export NETGENSRC=$MYSRCDIR/netgen-4.9.13 #for cffemlib compilation
    NGSOLVE_PATH=${MYSRCDIR}/ngsolve-4.9.13
    TOGL_PATH=${LOCSOFT}/lib/Togl1.7

    #sessa
    export SESSA_DATABASE_PATH=${GITR}/sessa-git/databases

    #parallel stuff (mpi + petsc + slepc)

    export MYMPI_INC_PATH=/usr/include/openmpi-x86_64
    export MYMPI_LIB_PATH=/usr/lib64/openmpi/lib
    export MYMPI_BIN_PATH=/usr/lib64/openmpi/bin

    export PETSC_DIR=${MYSRCDIR}/petsc-3.2-p6
    export PETSC_ARCH="arch-linux2-fft-complex-cxx-debug"

    #COMPILE PETSC
    export PETSC_MAIN_FLAGS="--with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-clanguage=cxx --with-shared-libraries=1 --with-fortran-kernels=1 --download-sowing --download-fftw=1"
    export PETSC_DEBUGGING="--with-debugging=1" #DEBUG BUILD
    # --with-blas-lapack-dir=/opt/intel/Compiler/11.1/046/mkl/lib
    export PETSC_OPT_FLAGS="CXXOPTFLAGS=-O3 COPTFLAGS=-O3 FOPTFLAGS=-03"
    # ./configure ${PETSC_MAIN_FLAGS} ${PETSC_OPT_FLAGS} ${PETSC_DEBUGGING}

    export SLEPC_DIR=${MYSRCDIR}/slepc-3.2-p3
    #./configure  #suffices

    #PETSC4PY
    PYSUFF="linux-x86_64-2.7" #Python suffix
    export PETSC4PY_DIR=${MYSRCDIR}/petsc4py-1.2
    P4PYLIB=${PETSC4PY_DIR}/build/temp.${PYSUFF}/$PETSC_ARCH/src
    P4PYPATH=${PETSC4PY_DIR}/build/lib.${PYSUFF} #/petsc4py

    #SLEPC4PY
    export SLEPC4PY_DIR=${MYSRCDIR}/slepc4py-1.2
    S4PYLIB=${SLEPCPY_DIR}/build/temp.${PYSUFF}/$PETSC_ARCH/src
    S4PYPATH=${SLEPCPY_DIR}/build/lib.${PYSUFF} #/slepc4py

    #MPI4PY
    PYMPIPATH=/usr/lib64/python2.7/site-packages/openmpi

    export LD_LIBRARY_PATH=${MYMPI_LIB_PATH}:${LOCSOFT}/lib:${P4PYLIB}:${S4PYLIB} #:${LD_LIBRARY_PATH}
    export EMBINPATH=${HOME}/gitrepos/emacs/src

    prepath ${LOCSOFT}/bin
    prepath ${MYMPI_BIN_PATH}
    prepath ${MATLAB_BIN}
    prepath ${EMBINPATH}
    prepath $HOME/.bin

elif [ "$HOSTNAME" = "dirac" -o "$HOSTNAME" = "dyson" ]; then
    prepath $HOME/bin
    prepath $HOME/.local/bin
    export EMBINPATH=${HOME}/gitrepos/emacs/src
    prepath ${EMBINPATH}
    export DDIR=${GITR}/diss

    export NETGENDIR=/opt/ngngs/bin
    export PYTHONPATH=$GITR/diss/task3:$DDIR/pysalt:${GITR}/cf-fem-lib/tools/in2d_creator_scripts:$PYTHONPATH

    # prepath $HOME/software/local/bin
    prepath $NETGENDIR
    prepath $GITR/diss/task3/scripts
    preldlpath $HOME/software/local/lib

    export CONDA_ENV_DIR=$HOME/miniconda/envs

    export MAKEOPTS='-j`nproc`'
    export PARDISO_LIB=/opt/libpardiso500-GNU481-X86-64.so
    ulimlimit=`free | grep '^Mem' | gawk '{ print(int($2 * 0.95)) }'`
    ulimit -v $ulimlimit
elif [ "$HOSTNAME" = "cobra" ]; then
    export PUBDOC=$HOME/gitrepos/publication
    DOTFPATH=$HOME/gitrepos/dotfiles

    export JDK_HOME=$HOME/sandbox/jdk1.7.0_40

    export MATLAB_BIN=/opt/MATLAB/R2012a/bin

    export RANDOMLAS=${GITR}/randomlas
    export CFFEM_REPO=${GITR}/cf-fem-lib

    export BOOST_SRC_PATH=$MYSRCDIR/boost_1_52_0

    export NETGENDIR=/opt/ngngs/bin

    export EMBINPATH=${HOME}/gitrepos/emacs/src

    export CONDA_ENV_DIR=$HOME/miniconda/envs

    # prepath $HOME/software/pycharm-community-3.4.1/bin
    prepath $EMBINPATH
    prepath $DOTFPATH/bin
    prepath $HOME/local/bin
    prepath $HOME/.local/bin
    prepath $HOME/bin
    prepath $HOME/.cabal/bin
    prepath $HOME/.cask/bin
    prepath $GITR/julia
    prepath $NETGENDIR

    export PYTHONPATH=${GITR}/diss/pysalt:${GITR}/diss/task3:$PYTHOPATH

elif [ "$HOSTNAME" = "thisch" ]; then
    intel_arch=""

    export MATLAB_BIN=/usr/local/MATLAB/R2012a/bin
    #export MATLAB_JAVA=/usr/lib/jvm/java-1.6.0-openjdk/jre

    export CFFEM_REPO=${MYSRCDIR}/cf-fem-lib
    #required by the main.m file
    export CFFEM_PREFIX=${CFFEM_REPO}
    export SALT_PATH=${CFFEM_REPO}
    export CFFEM_MLCODE=${CFFEM_REPO}/matlab
    export CFBD=${CFFEM_REPO}/build-release
    export CFBDMPI=${CFFEM_REPO}/build-mpi-release
    export RANDOMLAS=${CFFEM_REPO}/examples/2DFEM/randomlas
    export LAMBDAFOUR=${CFFEM_REPO}/examples/1DFDM/lambda4tests

    export PUBDOC=$GITR/publication
    export PUBSRC=$RANDOMLAS/
    export PUBEVAL=$RANDOMLAS/Publication

    #CFFEMLIB: NONMPI BUILD
    #LANG=C CC=icc CXX=icpc CXXFLAGS="-O3 -xHOST -openmp -ipo -gcc-name=gcc-4.5" cmake -DCMAKE_BUILD_TYPE=Release -DNETGEN_SOURCE_DIR=$NETGEN_SRC_PATH -DCMAKE_INSTALL_PREFIX=$LOCSOFT -DENABLE_NLOPT=1 -DCMAKE_EXE_LINKER_FLAGS="-shared-intel" ..

    #CFFEMLIB: MPI BUILD (verwendet noch den gcc)
    #TODO use intel compiler in mpicxx/mpicc !!
    #LANG=C CC=mpicc CXX=mpicxx CXXFLAGS="-O3 -march=native -fopenmp" cmake -DCMAKE_BUILD_TYPE=Release -DNETGEN_SOURCE_DIR=$NETGEN_SRC_PATH -DCMAKE_INSTALL_PREFIX=$LOCSOFT -DENABLE_NLOPT=1 -DENABLE_MPI=1 ..


    export NETGENDIR=${LOCSOFT}/bin #netgen needs this envvar
    export NETGEN_SRC_PATH=${GITR}/netgen/netgen
    #LANG=C CC=icc CXX=icpc CXXFLAGS="-O3 -xHOST -I$LOCSOFT/include -gcc-name=gcc-4.5" ./configure --prefix=$LOCSOFT --with-togl=$TOGL_PATH

    export NGSOLVE_PATH=${LOCSOFT}
    export NGSOLVE_SRC_PATH=${GITR}/ngsolve
    #OLD: compile ngsolve with (assuming that you have the config.site file in the prefix)
    #OLD: Does it make a difference if -O3 is in the CXXFLAGS in config.site or not ?!?!
    #autoreconf -i
    #LANG=C CC=icc CXX=icpc CXXFLAGS="-O3 -I$LOCSOFT/include -gcc-name=gcc-4.5" ./configure --prefix=$LOCSOFT

    export TOGL_PATH=${MYSRCDIR}/Togl-1.7
    export BOOST_PATH=${MYSRCDIR}/boost_1_50_0

    source /opt/intel/composer_xe_2011_sp1.10.319/bin/compilervars.sh intel64
    source /opt/intel/composer_xe_2011_sp1.11.339/bin/compilervars.sh intel64
    export INTEL_MKL_PREFIX=/opt/intel/composer_xe_2011_sp1.11.339/mkl

    export LD_LIBRARY_PATH=${LOCSOFT}/lib:/usr/local/lib:${LD_LIBRARY_PATH}

    export EMBINPATH=${HOME}/gitrepos/emacs/src
    export PATH=${MATLAB_BIN}:${HOME}/bin:${LOCSOFT}/bin:${EMBINPATH}:$PATH #EMACS-GIT
    # export PATH=${MATLAB_BIN}:${HOE}/bin:${LOCSOFT}/bin:$PATH

    #parallel stuff (mpi + petsc + slepc )

    export LD_LIBRARY_PATH=/usr/lib/openmpi/lib:$LD_LIBRARY_PATH


    # export PETSC_MAIN_FLAGS="--with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-clanguage=cxx --with-shared-libraries=1 --download-fftw=1 --with-fortran-kernels=1"
    export PETSC_MAIN_FLAGS="--with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-clanguage=cxx --with-shared-libraries=1 --with-fortran-kernels=1"

    # export PETSC_MUMPS_FLAGS="--download-mumps=1 --with-scalapack-lib=$INTEL_MKL_PREFIX/lib/intel64/libmkl_scalapack_lp64.a --with-scalapack-include=$INTEL_MKL_PREFIX/include --with-blacs-lib=[$INTEL_MKL_PREFIX/lib/intel64/libmkl_blacs_openmpi_lp64.a,$INTEL_MKL_PREFIX/lib/intel64/libmkl_core.a,$INTEL_MKL_PREFIX/lib/intel64/libmkl_blas95_lp64.a,$INTEL_MKL_PREFIX/lib/intel64/libmkl_gnu_thread.a,-lpthread] --with-blacs-include=$INTEL_MKL_PREFIX/include"
    # export PETSC_MUMPS_FLAGS="--with-blacs-lib=[$INTEL_MKL_PREFIX/lib/intel64/libmkl_blacs_openmpi_lp64.a,$INTEL_MKL_PREFIX/lib/intel64/libmkl_core.a,$INTEL_MKL_PREFIX/lib/intel64/libmkl_intel_thread.a,$INTEL_MKL_PREFIX/lib/intel64/libmkl_sequential.a,-lpthread] --with-blacs-include=$INTEL_MKL_PREFIX/include"

    export PETSC_MUMPS_FLAGS="--download-blacs=1 --download-mumps=1 --download-scalapack=1 --download-parmetis=1 --download-metis=1"
    export PETSC_DEBUGGING="--with-debugging=0" #RELEASE BUILD
    export PETSC_OPT_FLAGS="CXXOPTFLAGS='-O3' COPTFLAGS='-O3' FOPTFLAGS='-03'"
    export PETSC_BLAS_DIR="/opt/intel/composer_xe_2011_sp1.11.339/mkl/lib/intel64"

    #for light-matter
    export PETSC_DIR=${MYSRCDIR}/petsc-3.3-p2
    # export PETSC_DIR=${MYSRCDIR}/petsc-dev DOESNT WORK (checked out on 14.8.2012)
    export PETSC_ARCH=arch-linux2-cxx-mumps-release
    # export PETSC_ARCH=arch-linux2-cxx-release
    #wenn man slepc-dev vewendet muss man noch --download-sowing setzen
    #ERROR: cannot generate Fortran stubs; try configuring PETSc with --download-sowing or use a mercurial version of PETSc
    export PETSC_MAIN_FLAGS="${PETSC_MAIN_FLAGS} --download-sowing"


    # ./configure ${PETSC_MAIN_FLAGS} --with-blas-lapack-dir=${PETSC_BLAS_DIR} ${PETSC_OPT_FLAGS} ${PETSC_MUMPS_FLAGS} ${PETSC_DEBUGGING}

    #FOR LIGHT-MATTER PROJECT
    #./configure ${PETSC_MAIN_FLAGS} --with-blas-lapack-dir=${PETSC_BLAS_DIR} ${PETSC_OPT_FLAGS} ${PETSC_MUMPS_FLAGS} ${PETSC_DEBUGGING} --download-fftw=$PETSC_DIR/fftw-3.3.2.tar.gz

    # export PETSC_ARCH=arch-linux2-intel-cxx-debug #-> fehlermeldung cffemlib compile
    #./configure --with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-c-support=1 CXXOPTFLAGS="-O3" COPTFLAGS="-O3" FOPTFLAGS="-03" --download-fftw=$PETSC_DIR/fftw-3.3.2.tar.gz --with-blas-lapack-dir=/opt/intel/composerxe/mkl/lib/intel64 --with-shared-libraries=1
    #HIER NOCH MIT INTEL COMPILERN (geht im moment nicht)
    #./configure --with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-c-support=1 --with-cc=icpc -with-g++=icc CXXOPTFLAGS="-O3 -xHOST" COPTFLAGS="-O3 -xHOST" FOPTFLAGS="-03 -xHOST" --download-fftw=$PETSC_DIR/fftw-3.3.2.tar.gz --with-blas-lapack-dir=/opt/intel/composerxe/mkl/lib/intel64 --with-shared-libraries=1


    export SLEPC_DIR=${MYSRCDIR}/slepc-dev
    #./configure  #suffices

    #PETSC4PY
    export PETSC4PY_DIR=${MYSRCDIR}/petsc4py-dev
    # P4PYLIB=${PETSCPY_DIR}/build/temp.linux-x86_64-2.7/arch-linux2-cxx-release/src
    P4PYLIB=${PETSC4PY_DIR}/build/temp.linux-x86_64-2.7/$PETSC_ARCH/src
    P4PYPATH=${PETSC4PY_DIR}/build/lib.linux-x86_64-2.7 #/petsc4py

    #SLEPC4PY
    export SLEPC4PY_DIR=${MYSRCDIR}/slepc4py-dev
    # S4PYLIB=${SLEPCPY_DIR}/build/temp.linux-x86_64-2.7/arch-linux2-cxx-release/src
    S4PYLIB=${SLEPC4PY_DIR}/build/temp.linux-x86_64-2.7/arch-linux2-intel-cxx-debug/src
    S4PYPATH=${SLEPC4PY_DIR}/build/lib.linux-x86_64-2.7 #/slepc4py

    #MPI4PY (do I need this ??)
    PYMPIPATH=/usr/lib/python2.7/dist-packages/mpi4py


    LMPRO=${GITR}/light-matter/src

    export LD_LIBRARY_PATH=${P4PYLIB}:${S4PYLIB}:${LD_LIBRARY_PATH}

    #NLOPT: in order to create python modules you need to pass the
    #--enable-shared option to the configure script
    #CXXFLAGS='-fPIC' CFLAGS='-fPIC' ./configure --prefix=${LOCSOFT} --enable-shared --without-matlab --with-cxx
    #make && make install

else
    arch=""
fi

if [ "$arch" ]; then
    . ${intel_prefix}${intel_version}/bin/$arch/iccvars_$arch.sh
    . ${intel_prefix}${intel_version}/bin/$arch/ifortvars_$arch.sh
fi

if [ "${PUBDOC}" ]; then
    hash -d doc=${PUBDOC}
    hash -d pubdoc=${PUBDOC}
fi

if [ "$HOSTNAME" = "thisch" -o "$HOSTNAME" = "mustang" ]; then
    if [ -z "${CFFEM_REPO}" ]; then
        echo Warning CFFEM_REPO not defined
    else
        hash -d cfrepo=${CFFEM_REPO}
        hash -d cfr=${CFFEM_REPO}
    fi

    #cffemlib + simulation stuff
    export PYTHONPATH=${CFFEM_REPO}/tools/in2d_creator_scripts:${RANDOMLAS}/scripts:${RANDOMLAS}:${LOCSOFT}/lib/python2.7/site-packages
    if [ "${GITR}" ]; then
        if [ "${LMPRO}" ]; then
            if [ -d "${LMPRO}" ]; then
                export PYTHONPATH=${LMPRO}:${PYTHONPATH}
            fi
        fi
    fi

    if [ "${P4PYPATH}" ]; then
            export PYTHONPATH=${P4PYPATH}:${PYTHONPATH}
    fi
    if [ "${S4PYPATH}" ]; then
            export PYTHONPATH=${S4PYPATH}:${PYTHONPATH}
    fi
    if [ "${PYMPIPATH}" ]; then
            export PYTHONPATH=${PYMPIPATH}:${PYTHONPATH}
    fi

    export PATH=${RANDOMLAS}:${RANDOMLAS}/scripts:${RANDOMLAS}/testscripts:${CFBDMPI}/green:${CFBDMPI}/src:${PATH}

    if [ "${NGSOLVE_PATH}" ]; then
        export LD_LIBRARY_PATH=${NGSOLVE_PATH}/lib:${LD_LIBRARY_PATH}
    fi
    if [ "${TOGL_PATH}" ]; then
        export LD_LIBRARY_PATH=${TOGL_PATH}:${LD_LIBRARY_PATH}
    fi

    export LD_LIBRARY_PATH=${CFBDMPI}/lib:${LD_LIBRARY_PATH}
fi

. ~/.zsh/linuxconsole

#[[ $- != *i* ]] && return
#[[ -z "$TMUX" ]] && exec tmux
