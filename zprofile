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
export HISTCONTROL=ignoredups
export IGNOREEOF=3
export AWT_TOOLKIT=MToolkit # for matlab
HOSTNAME=`hostname`

ONVSC=`hostname | egrep '(l01|r[0-9]+{2}n[0-9]+{2})' `

# Exports
if [ -z "$ONVSC" ]; then
    export PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin
else
    echo "loading zprofile"
    echo onvsc: $ONVSC
fi

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

    export BROWSER=google-chrome

    export MYSRCDIR=$HOME/local/src
    export LOCSOFT=$HOME/local/software

    export GITR=$HOME/gitrepos
    export TUDADOC=$GITR/tudadoc

    export MATLAB_BIN=/usr/local/MATLAB/R2010b/bin
    export MATLAB_JAVA=/usr/lib/jvm/java-1.6.0-openjdk-1.6.0.0.x86_64/jre

    export PYCHARM_JDK=$MATLAB_JAVA

    export CFFEM_REPO=${HOME}/cf-fem-lib
    export CFFEM_MLCODE=${CFFEM_REPO}/matlab
    export CFBD=${CFFEM_REPO}/build
    export RANDOMLAS=${HOME}/gitrepos/randomlas
    export LAMBDAFOUR=${CFFEM_REPO}/examples/1DFDM/lambda4tests

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

    #PETSC4PY
    PETSCPY_DIR=${MYSRCDIR}/petsc4py-1.2
    P4PYLIB=${PETSCPY_DIR}/build/temp.linux-x86_64-2.7/arch-linux2-cxx-debug/src
    P4PYPATH=${PETSCPY_DIR}/build/lib.linux-x86_64-2.7 #/petsc4py

    #SLEPC4PY
    SLEPCPY_DIR=${MYSRCDIR}/slepc4py-1.2
    S4PYLIB=${SLEPCPY_DIR}/build/temp.linux-x86_64-2.7/arch-linux2-cxx-debug/src
    S4PYPATH=${SLEPCPY_DIR}/build/lib.linux-x86_64-2.7 #/slepc4py

    #MPI4PY
    PYMPIPATH=/usr/lib64/python2.7/site-packages/openmpi

    export LD_LIBRARY_PATH=${MYMPI_LIB_PATH}:${LOCSOFT}/lib:${P4PYLIB}:${S4PYLIB} #:${LD_LIBRARY_PATH}
    # export EMBINPATH=${HOME}/gitrepos/emacs/src
    export PATH=${LOCSOFT}/bin:${MYMPI_BIN_PATH}:${MATLAB_BIN}:${EMBINPATH}:$HOME/qtcreator-2.5.0/bin/:$PATH

elif [ "$HOSTNAME" = "cobra" ]; then
    DOTFPATH=$HOME/gitrepos/dotfiles
    export LOCSOFT=$HOME/local/software
    export TUDADOC=$HOME/gitrepos/tudadoc
    export PATH=$DOTFPATH/bin:$LOCSOFT/idlex-0.8/:$PATH
    export BROWSER=google-chrome

    #parallel stuff (mpi + petsc + slepc)

    export MYMPI_INC_PATH=/usr/include/openmpi-x86_64
    export MYMPI_LIB_PATH=/usr/lib64/openmpi/lib
    export MYMPI_BIN_PATH=/usr/lib64/openmpi/bin

    export MYSRCDIR=$HOME/local/src
    export PETSC_DIR=${MYSRCDIR}/petsc-3.2-p7
    export PETSC_ARCH="arch-linux2-c"
    export SLEPC_DIR=${MYSRCDIR}/slepc-3.2-p3

    SCIPY_DIR=$HOME/gitrepos/scipy
    SCIPYLIB=${SCIPY_DIR}/build/temp.linux-x86_64-2.7
    SCIPYPATH=${SCIPY_DIR}/build/lib.linux-x86_64-2.7

    export PYTHONPATH=${SCIPYPATH}:"/usr/lib64/python2.7/site-packages/openmpi"
    export LD_LIBRARY_PATH=${SCIPYLIB}:${MYMPI_LIB_PATH}:${LOCSOFT}/lib
    export PATH=${LOCSOFT}/bin:${MYMPI_BIN_PATH}:${HOME}/qtcreator-2.5.0/bin:$PATH

elif [ "$HOSTNAME" = "thisch" ]; then
    #arch="intel64"
    intel_arch="" #"intel64"
    #intel_version=""
    #intel_prefix="/home/thomas/intel"

    export BROWSER=chromium-browser

    export MYSRCDIR=${HOME}/local/src
    export LOCSOFT=$HOME/local/software

    export GITR=$HOME/gitrepos
    export TUDADOC=$GITR/tudadoc

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

    #TODO compile instructions cf-fem-lib

    #single core version:
    # cmake -DCMAKE_CXX_FLAGS=-I{$MYSRCDIR} -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=${LOCSOFT} -DBOOST_ROOT=${BOOST_PATH} ..

    #multi core version:
    # cmake -DCMAKE_CXX_FLAGS=-I{$MYSRCDIR} -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=${LOCSOFT} -DBOOST_ROOT=${BOOST_PATH} -DENABLE_MPI=1 ..

    #intel single core version:
    #LANG=C CC=icc CXX=icpc cmake -DCMAKE_CXX_FLAGS=-I${MYSRCDIR} -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=${LOCSOFT} -DBOOST_ROOT=${BOOST_PATH} ..


    export NETGENDIR=${LOCSOFT}/bin #netgen needs this envvar
    export NETGEN_SRC_PATH=${GITR}/netgen

    export NGSOLVE_PATH=${LOCSOFT}
    export NGSOLVE_SRC_PATH=${GITR}/ngsolve
    #compile ngsolve with (assuming that you have the config.site file in the prefix)
    #Does it make a difference if -O3 is in the CXXFLAGS in config.site or not ?!?!

    #BRAUCHEN WIR ÜBERHAUPT DIE MKL ??
    #I don't know if the MKL stuff works in this case
    #./configure --prefix=${LOCSOFT} --with-netgen=${LOCSOFT} --with-lapack='-lsvml -lmkl_intel_lp64 -lmkl_sequential -lmkl_core'

    TOGL_PATH=${MYSRCDIR}/Togl-1.7

    export BOOST_PATH=${MYSRCDIR}/boost_1_50_0

    source /opt/intel/composer_xe_2011_sp1.10.319/bin/compilervars.sh intel64
    source /opt/intel/composer_xe_2011_sp1.11.339/bin/compilervars.sh intel64

    export LD_LIBRARY_PATH=${LOCSOFT}/lib:/usr/local/lib:${LD_LIBRARY_PATH}

    # export EMBINPATH=${HOME}/gitrepos/emacs/src
    # export PATH=${MATLAB_BIN}:${HOME}/bin:${LOCSOFT}/bin:${EMBINPATH}:$PATH #EMACS-GIT
    export PATH=${MATLAB_BIN}:${HOME}/bin:${LOCSOFT}/bin:$PATH

    #parallel stuff (mpi + petsc + slepc )

    export LD_LIBRARY_PATH=/usr/lib/openmpi/lib:$LD_LIBRARY_PATH


    export PETSC_MAIN_FLAGS="--with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-clanguage=cxx --with-shared-libraries=1 --download-fftw=1 --with-fortran-kernels=1"

    export PETSC_DEBUGGING="--with-debuggging=0" #RELEASE BUILD
    export PETSC_OPT_FLAGS="CXXOPTFLAGS='-O3' COPTFLAGS='-O3' FOPTFLAGS='-03'"

    export PETSC_BLAS_DIR="/opt/intel/composer_xe_2011_sp1.11.339/mkl/lib/intel64"

    # export PETSC_DIR=${MYSRCDIR}/petsc-dev
    # export PETSC_ARCH=arch-linux2-cxx-debug

    #for light-matter
    export PETSC_DIR=${MYSRCDIR}/petsc-3.3-p2
    export PETSC_ARCH=arch-linux2-cxx-release
    #wenn man slepc-dev vewendet muss man noch --download-sowing setzen
    #ERROR: cannot generate Fortran stubs; try configuring PETSc with --download-sowing or use a mercurial version of PETSc
    export PETSC_MAIN_FLAGS="${PETSC_MAIN_FLAGS} --download-sowing"


    # ./configure ${PETSC_MAIN_FLAGS} --with-blas-lapack-dir=${PETSC_BLAS_DIR} ${PETSC_OPT_FLAGS} ${PETSC_DEBUGGING}


    #./configure ${PETSC_MAIN_FLAGS} --with-blas-lapack-dir=${PETSC_BLAS_DIR} CXXOPTFLAGS="-O3" COPTFLAGS="-O3" FOPTFLAGS="-03" --with-debugging=0 --download-fftw=$PETSC_DIR/fftw-3.3.2.tar.gz

    # export PETSC_ARCH=arch-linux2-intel-cxx-debug #-> fehlermeldung cffemlib compile
    #./configure --with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-c-support=1 CXXOPTFLAGS="-O3" COPTFLAGS="-O3" FOPTFLAGS="-03" --download-fftw=$PETSC_DIR/fftw-3.3.2.tar.gz --with-blas-lapack-dir=/opt/intel/composerxe/mkl/lib/intel64 --with-shared-libraries=1
    #HIER NOCH MIT INTEL COMPILERN (geht im moment nicht)
    #./configure --with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-c-support=1 --with-cc=icpc -with-g++=icc CXXOPTFLAGS="-O3 -xHOST" COPTFLAGS="-O3 -xHOST" FOPTFLAGS="-03 -xHOST" --download-fftw=$PETSC_DIR/fftw-3.3.2.tar.gz --with-blas-lapack-dir=/opt/intel/composerxe/mkl/lib/intel64 --with-shared-libraries=1


    export SLEPC_DIR=${MYSRCDIR}/slepc-dev
    #./configure  #suffices

    #PETSC4PY
    PETSCPY_DIR=${MYSRCDIR}/petsc4py-dev
    # P4PYLIB=${PETSCPY_DIR}/build/temp.linux-x86_64-2.7/arch-linux2-cxx-release/src
    P4PYLIB=${PETSCPY_DIR}/build/temp.linux-x86_64-2.7/$PETSC_ARCH/src
    P4PYPATH=${PETSCPY_DIR}/build/lib.linux-x86_64-2.7 #/petsc4py

    #SLEPC4PY
    SLEPCPY_DIR=${MYSRCDIR}/slepc4py-dev
    # S4PYLIB=${SLEPCPY_DIR}/build/temp.linux-x86_64-2.7/arch-linux2-cxx-release/src
    S4PYLIB=${SLEPCPY_DIR}/build/temp.linux-x86_64-2.7/arch-linux2-intel-cxx-debug/src
    S4PYPATH=${SLEPCPY_DIR}/build/lib.linux-x86_64-2.7 #/slepc4py

    #MPI4PY (do I need this ??)
    PYMPIPATH=/usr/lib/python2.7/dist-packages/mpi4py


    LMPRO=${GITR}/light-matter/src

    export LD_LIBRARY_PATH=${P4PYLIB}:${S4PYLIB}:${LD_LIBRARY_PATH}

    #NLOPT: in order to create python modules you need to pass the
    #--enable-shared option to the configure script
    #CXXFLAGS='-fPIC' CFLAGS='-fPIC' ./configure --prefix=${LOCSOFT} --enable-shared --without-matlab
    #make && make install

    #EMACS:
    # ./configure --with-gif=no --with-tiff=no --with-x-toolkit=gtk3


elif [ "$ONVSC" ]; then
    arch=""

    export LANG="C"
    export LC_ALL="C"

    export MYSRCDIR=$HOME/local/src
    export LOCSOFT=$HOME/local/software

    export CFFEM_REPO=${HOME}/gitrepos/cf-fem-lib
    export CFBD=${CFFEM_REPO}/build_release
    export CFBDMPI=${CFFEM_REPO}/build_release

    export TOGL_PATH=${LOCSOFT}/lib/Togl1.7
    export NETGENDIR=${LOCSOFT}/bin
    export NETGEN_SRC_PATH=${MYSRCDIR}/netgen_with_icc/netgen #version from ml
    #LANG=C CC=icc CXX=icpc CXXFLAGS="-O3 -xHOST -I$LOCSOFT/include" ./configure --prefix=$LOCSOFT --with-togl=$TOGL_PATH

    export aNGSOLVE_SRC_PATH=${MYSRCDIR}/ngsolve-dev/ngsolve
    #libtoolize && autoreconf && automake --add-missing && autoreconf
    #LANG=C CC=icc CXX=icpc CXXFLAGS="-O3 -I$LOCSOFT/include" ./configure --prefix=$LOCSOFT

    export RANDOMLAS=$HOME/gitrepos/randomlas

    #python distribution
    export EPDPATH=$LOCSOFT/epd-7.2-1-rh5-x86_64/bin

    #MPI stuff
    #TODO use appropriate includes set by mpi-selector
    export MYMPI_INC_PATH=/usr/mpi/qlogic/include
    export MYMPI_LIB_PATH=/usr/mpi/qlogic/lib64

    export BOOST_SRC_PATH=$MYSRCDIR/boost_1_50_0

    export PETSC_MAIN_FLAGS="--with-c++-support=1 --with-scalar-type=complex --with-x11=0 --with-clanguage=cxx --with-shared-libraries=1 --with-fortran-kernels=1"
    export PETSC_DEBUGGING="--with-debuggging=0" #RELEASE BUILD
    export PETSC_OPT_FLAGS="CXXOPTFLAGS='-O3 -xHOST' COPTFLAGS='-O3 -xHOST' FOPTFLAGS='-O3-xHOST'"
    export PETSC_BLAS_DIR="/opt/intel/Compiler/11.1/046/mkl/lib"

    #wenn man slepc-dev vewendet muss man noch --download-sowing setzen
    #ERROR: cannot generate Fortran stubs; try configuring PETSc with --download-sowing or use a mercurial version of PETSc
    export PETSC_MAIN_FLAGS="${PETSC_MAIN_FLAGS} --download-sowing"

    # ./configure ${PETSC_MAIN_FLAGS} --with-blas-lapack-dir=${PETSC_BLAS_DIR} ${PETSC_OPT_FLAGS} ${PETSC_DEBUGGING}

    export PETSC_DIR=${MYSRCDIR}/petsc-3.3-p2
    #export PETSC_ARCH="arch-linux2-cxx-debug"
    export PETSC_ARCH=intel-cxx-complex_release
    export SLEPC_DIR=${MYSRCDIR}/slepc-dev

    export EMBINPATH=/usr/local/bin
    export PATH=$HOME/bin:${LOCSOFT}/bin:$EPDPATH:$PATH
    export LD_LIBRARY_PATH=${LOCSOFT}/lib/:${LD_LIBRARY_PATH}

    #for the xml_pp program
    export PERLLIB=/home/lv70072/thisch/bin/

else
    arch=""
fi

#if [[ "$arch" = "ia32" ]] || [[ "$arch" = "intel64" ]]; then
if [ "$arch" ]; then
    . ${intel_prefix}${intel_version}/bin/$arch/iccvars_$arch.sh
    . ${intel_prefix}${intel_version}/bin/$arch/ifortvars_$arch.sh
fi

if [ "${TUDADOC}" ]; then
        hash -d doc=${TUDADOC}
fi

if [ "$HOSTNAME" = "thisch" -o -n "$ONVSC" -o "$HOSTNAME" = "mustang" ]; then
    if [ -z "${RANDOMLAS}" ]; then
        echo Warning RANDOMLAS not defined
    else
        hash -d rand=${RANDOMLAS}
    fi
    if [ -z "${LAMBDAFOUR}" ]; then
        echo Warning RANDOMLAS not defined
    else
        hash -d lamb=${LAMBDAFOUR}
    fi
    if [ -z "${CFFEM_REPO}" ]; then
        echo Warning CFFEM_REPO not defined
    else
        hash -d cfrepo=${CFFEM_REPO}
        hash -d cfr=${CFFEM_REPO}
    fi

    #cffemlib + simulation stuff
    export PYTHONPATH=${CFFEM_REPO}/tools/in2d_creator_scripts:${RANDOMLAS}/scripts:${RANDOMLAS}:${LOCSOFT}/lib/python2.7/site-packages
    if [ "${GITR}" ]; then
        if [ -d "${GITR}/matplotlib2tikz" ]; then
            export PYTHONPATH=${GITR}/matplotlib2tikz:${PYTHONPATH}
        fi
        if [ -d "${GITR}/matlab2tikz" ]; then
            export MATLAB2TIKZ=${GITR}/matlab2tikz
        fi
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

    export PATH=${RANDOMLAS}:${RANDOMLAS}/scripts:${CFBDMPI}/green:${CFBDMPI}/src:${PATH}

    if [ "${NGSOLVE_PATH}" ]; then
        export LD_LIBRARY_PATH=${NGSOLVE_PATH}/lib:${LD_LIBRARY_PATH}
    fi
    if [ "${TOGL_PATH}" ]; then
        export LD_LIBRARY_PATH=${TOGL_PATH}:${LD_LIBRARY_PATH}
    fi

    export LD_LIBRARY_PATH=${CFBDMPI}/lib:${LD_LIBRARY_PATH}

fi
# watch for people
#watch=(notme)                   # watch for everybody but me
#LOGCHECK=300                    # check every 5 min for login/logout activity

. ~/.zsh/linuxconsole
