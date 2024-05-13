#!/bin/sh
set -eu

# set default values
  CLEAN="noclean"

# parse arguments
  for arg in "$@"
  do
      case $arg in
          clean)
          CLEAN="clean"
          shift # Remove --initialize from processing
          ;;
      esac
  done

# set up some default variables for use within the helper scripts
  export BUILD_ROOT=${PWD}
  export FV3_DA_SRC=${PWD%/*/*}/src/
  export PATH="${BUILD_ROOT}/site:${BUILD_ROOT}/mk_scripts:${PATH}"

# conditionally clean the build directory
  if [ ${CLEAN} = "clean" ] ; then
     echo " cleaning build directory in 2 seconds"
     sleep 2
     \rm -rf exec/*
  fi

# load the proper environment for your machine

 . ${BUILD_ROOT}/site/environment.sh

# ensure the build and final executable locations are available
  mkdir -p ./exec
  mkdir -p ./bin

#
# configure your build parameters
# options
#    comp:  prod, repro, debug
#     bit:  32bit, 64bit
#     avx:  Y, N               ! use AVX_LEVEl defined in site/environment.sh
#                              ! default AVX_LEVEL: CORE-AVX-I
#

  comp="prod"
  bit="32bit"
  avx="N"
  echo -e "  building ${comp} ${bit} \t `date`"
    mk_paths                        > build_${comp}.${bit}.out 2>&1    # create the file list for the build
    mk_makefile ${bit}             >> build_${comp}.${bit}.out 2>&1    # create the library makefiles
    mk_make ${comp} ${bit} ${avx}  >> build_${comp}.${bit}.out 2>&1    # build the configuration
    mv -f exec/test.x bin/fv3_da_out_${bit}.x                          # move the executable to an accessible area

# test and report on build success
  if [ $? -ne 0 ] ; then
     echo ">>> cube2gaus build ${comp} ${bit} failed"
     exit 1
  else
     echo " cube2gaus build ${comp} ${bit} successful"
  fi

exit 0
