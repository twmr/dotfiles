# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

function initzsh {
	echo "loading zsh"
	if [ -f ~/.zprofile ]; then
	  . ~/.zprofile
	fi
	exec zsh
}

function initcfbuild {
    LANG=C CC=mpicc CXX=mpicxx  cmake -DBOOST_ROOT=$BOOST_SRC_PATH \
        -DCMAKE_CXX_FLAGS="-I$MYSRCDIR" -DCMAKE_EXE_LINKER_FLAGS="-shared-intel" \
        -DCMAKE_BUILD_TYPE=Release -DNETGEN_SOURCE_DIR=$NETGENSRCDIR \
        -DENABLE_MPI=1 \
        -DENABLE_NLOPT=1 \
        -DMPI_INCLUDE_PATH=$MYMPI_INC_PATH -DMPI_LIBRARY=$MYMPI_LIB_PATH \
        -DCMAKE_INSTALL_PREFIX=$LOCSOFT ..
    make -j8
}

function initnonmpicfbuild {
    LANG=C cmake -DBOOST_ROOT=$BOOST_SRC_PATH \
        -DCMAKE_CXX_FLAGS="-I$MYSRCDIR" \
        -DCMAKE_BUILD_TYPE=Release -DNETGEN_SOURCE_DIR=$NETGENSRCDIR \
        -DCMAKE_INSTALL_PREFIX=$LOCSOFT ..
}

function buildpetsc {
   # ./configure --with-scalar-type=complex --with-boost-dir=$BOOST_SRC_PATH --with-X11=0
   # ./configure --with-c++-support=1   --with-scalar-type=complex  --with-x11=0 \
   #     --with-c-support=1 --with-blas-lapack-dir=/opt/intel/Compiler/11.1/046/mkl/lib \
   #     CXXOPTFLAGS="-O3 -xHOST" COPTFLAGS="-O3 -xHOST" FOPTFLAGS="-03 -xHOST"

   #new shared library stuff

    echo number of args $#

    if [ "$1" == "DEBUG" ]; then
        extraflags="PETSC_ARCH=intel-cxx-complex_debug --with-debugging=1";
    else
        extraflags="PETSC_ARCH=intel-cxx-complex --with-debugging=0 CXXOPTFLAGS='-O3 -xHost' COPTFLAGS='-O3 -xHost' FOPTFLAGS='-03 -xHost'"
    fi
    echo $extraflags
    # return -1

    cd $PETSC_DIR
    ./configure --with-c++-support=1 --with-scalar-type=complex --with-x11=0 \
        --with-clanguage=cxx --with-blas-lapack-dir=/opt/intel/Compiler/11.1/046/mkl/lib \
        CXXOPTFLAGS="-O3" COPTFLAGS="-O3" FOPTFLAGS="-03" \
        --with-shared-libraries=1 ${extraflags}

    #todo play with:
    #'--download-parmetis=yes',
    #'--download-plapack=yes',
    #'--download-superlu_dist=yes',
    #'--download-mumps=yes',
    #'--download-spooles=yes',
    #'--with-fortran-kernels=1',
    #'--download-blacs=ifneeded',
    #'--download-scalapack=ifneeded',
}

function buildslepc {
    cd $SLEPC_DIR
    ./configure
}

function  makecfmpi {
    cd $CFBD; make -j8 pertubation; cd -
}

function makecfmpiall {
    cd $CFBD; make -j8 ; cd -
}

# function makecfmpi {
#     cd $CFBD; make -j8 pertubation; cd -
# }
