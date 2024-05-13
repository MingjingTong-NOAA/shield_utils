#!/bin/sh

if [ `hostname | cut -c1-5` = "Orion" ] ; then
   echo " Orion environment "

   . ${MODULESHOME}/init/sh
   module load intel/2020
   module load impi/2020
   module load netcdf
   module load hdf5

   export CPATH="${NETCDF}/include:${CPATH}"
   export HDF5=${HDF5_ROOT}
   export LIBRARY_PATH="${LIBRARY_PATH}:${NETCDF}/lib:${HDF5}/lib"
   export NETCDF_DIR=${NETCDF}

   # make your compiler selections here
   export FC=mpiifort
   export CC=mpiicc
   export CXX=mpicpc
   export LD=mpiifort
   export TEMPLATE=site/intel.mk

   # highest level of AVX support
   export AVX_LEVEL=-xSKYLAKE-AVX512


elif [ `hostname | cut -c1-4` = "gaea" ] ; then
   echo " gaea environment "

   . ${MODULESHOME}/init/sh
   module unload PrgEnv-pgi
   module load   PrgEnv-intel
   module rm intel
   module load intel/19.0.5.281
   module load cray-netcdf
   module load craype-hugepages4M

   # make your compiler selections here
   export FC=ftn
   export CC=cc
   export CXX=CC
   export LD=ftn
   export TEMPLATE=site/intel.mk

   # highest level of AVX support
   export AVX_LEVEL=-xCORE-AVX2


elif [ `hostname | cut -c1-2` = "fe" ] || [ `hostname | cut -c1` = "x" ] ; then
   echo " jet environment "

   . ${MODULESHOME}/init/sh
   module purge
   module load newdefaults
   module load intel/2016.2.181 # Jet's default is 15.0.3.187, but this one is 16.0.2.181
   module load szip/2.1
   module load hdf5/1.8.9
   module load netcdf4/4.2.1.1
   module load mvapich2/2.1

   export LIBRARY_PATH="${LIBRARY_PATH}:${NETCDF4}/lib:${HDF5}/lib"
   export NETCDF_DIR=${NETCDF4}

   # make your compiler selections here
   export FC=mpiifort
   export CC=mpiicc
   export CXX=mpicpc
   export LD=mpiifort
   export TEMPLATE=site/intel.mk


elif [ `hostname | cut -c1` = "h" ] ; then
   echo " hera environment "

   source $MODULESHOME/init/sh
   module purge
   source $MODULESHOME/init/sh
   module purge
   module use /scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack
   module load hpc/1.1.0
   module load hpc-intel/18.0.5.274
   module load hpc-impi/2018.0.4

   module load hdf5
   module load netcdf

   module load bacio
   module load sp
   module load w3emc
   module load nemsio
   module list

   export CPATH="$NETCDF/include:$CPATH"
   export HDF5=${HDF5_ROOT}
   export LIBRARY_PATH="${LIBRARY_PATH}:${NETCDF}/lib:${HDF5}/lib"
   export NETCDF_DIR=${NETCDF}

   # make your compiler selections here
   export FC=mpiifort
   export CC=mpiicc
   export CXX=mpicpc
   export LD=mpiifort
   export TEMPLATE=site/intel.mk

   # highest level of AVX support
   export AVX_LEVEL=-xSKYLAKE-AVX512
   module list
else

   echo " no environment available based on the hostname "

fi

