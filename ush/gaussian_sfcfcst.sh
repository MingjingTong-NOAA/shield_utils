#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         gaussian_sfcfcst.sh
# Script description:  Makes a global gaussian grid surface forecast file
#
# Author:        George Gayno       Org: NP23         Date: 2018-01-30
#
# Abstract: This script makes a global gaussian grid surface forecast from
#           fv3gfs surface forecast tiles
#
# Script history log:
# 2018-01-30  Gayno  initial script
# 2021-02-18  Mingjing Tong modified to process forecast files for SHiELD
#
# Usage:  gaussian_sfcfcst.sh
#
#   Imported Shell Variables:
#     CASE          Model resolution.  Defaults to C768.
#     DONST         Process NST fields when 'yes'.  Default is 'no'.
#     OUTPUT_FILE   Output gaussian forecast file format.  Default is "nemsio"
#                   Set to "netcdf" for netcdf output file
#                   Otherwise, output in nemsio.
#     BASEDIR       Root directory where all scripts and fixed files reside.
#                   Default is /nwprod2.
#     HOMEgfs       Directory for gfs version.  Default is
#                   $BASEDIR/gfs_ver.v15.0.0}
#     FIXam         Directory for the global fixed climatology files.
#                   Defaults to $HOMEgfs/fix/am
#     FIXfv3        Directory for the model grid and orography netcdf
#                   files.  Defaults to $HOMEgfs/fix/orog
#     FIXWGTS       Weight file to use for interpolation
#     EXECgfs       Directory of the program executable.  Defaults to
#                   $HOMEgfs/exec
#     DATA          Working directory
#                   (if nonexistent will be made, used and deleted)
#                   Defaults to current working directory
#     COMOUT        Output directory
#                   (if nonexistent will be made)
#                   defaults to current working directory
#     XC            Suffix to add to executables. Defaults to none.
#     GAUSFCFCSTEXE  Program executable.
#                   Defaults to $EXECgfs/gaussian_sfcfcst.exe
#     INISCRIPT     Preprocessing script.  Defaults to none.
#     LOGSCRIPT     Log posting script.  Defaults to none.
#     ERRSCRIPT     Error processing script
#                   defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT     Postprocessing script
#                   defaults to none
#     CDATE         Output forecast date in yyyymmddhh format. Required.
#     PGMOUT        Executable standard output
#                   defaults to $pgmout, then to '&1'
#     PGMERR        Executable standard error
#                   defaults to $pgmerr, then to '&1'
#     pgmout        Executable standard output default
#     pgmerr        Executable standard error default
#     REDOUT        standard output redirect ('1>' or '1>>')
#                   defaults to '1>', or to '1>>' to append if $PGMOUT is a file
#     REDERR        standard error redirect ('2>' or '2>>')
#                   defaults to '2>', or to '2>>' to append if $PGMERR is a file
#     VERBOSE       Verbose flag (YES or NO)
#                   defaults to NO
#     gfs_ver       Version number of gfs directory.  Default is
#                   v15.0.0.
#     OMP_NUM_
#     THREADS_SFC   Number of omp threads to use.  Default is 1.
#     APRUNSFC      Machine specific command to invoke the executable.
#                   Default is none.
#
#   Exported Shell Variables:
#     PGM           Current program name
#     pgm
#     ERR           Last return code
#     err
#
#   Modules and files referenced:
#     scripts    : $INISCRIPT
#                  $LOGSCRIPT
#                  $ERRSCRIPT
#                  $ENDSCRIPT
#
#     programs   : $GAUSFCFCSTEXE
#
#     fixed data : $FIXfv3/${CASE}/${CASE}_oro_data.tile*.nc
#                  $FIXWGTS
#                  $FIXam/global_hyblev.l65.txt
#
#     input data : $COMOUT/RESTART/${PDY}.${cyc}0000.sfcf*_data.tile*.nc
#
#     output data: $PGMOUT
#                  $PGMERR
#                  $COMOUT/${APREFIX}sfcf${ASUFFIX}
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
#  Control variable resolution priority
#    1 Command line argument.
#    2 Environment variable.
#    3 Inline default.
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
#
################################################################################

source "$HOMEgfs/ush/preamble.sh"

VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
   exec >> $DATA/logf$( printf "%03d" $fhour) 2>&1
fi

CASE=${CASE:-C768}
res=$(echo $CASE | cut -c2-)
LONB_CASE=$((res*4))
LATB_CASE=$((res*2))
LONB_SFC=${LONB_SFC:-$LONB_CASE}
LATB_SFC=${LATB_SFC:-$LATB_CASE}
DONST=${DONST:-"NO"}
SFC4POST=${SFC4POST:-".true"}
LEVS=${LEVS:-91}
OUTPUT_FILE=${OUTPUT_FILE:-"netcdf"}
if [ $OUTPUT_FILE = "netcdf" ]; then
    export NETCDF_OUT=".true."
else
    export NETCDF_OUT=".false."
fi

#  Directories.
gfs_ver=${gfs_ver:-v15.0.0}
BASEDIR=${BASEDIR:-${NWROOT:-/nwprod2}}
HOMEgfs=${HOMEgfs:-$BASEDIR/gfs_ver.${gfs_ver}}
EXECgfs=${EXECgfs:-$HOMEgfs/exec}
FIXfv3=${FIXfv3:-$HOMEgfs/fix/orog}
FIXam=${FIXam:-$HOMEgfs/fix/am}
FIXshield=${FIXshield:-$HOMEgfs/fix/shield}
FIXGAUS=${FIXGAUS:-$HOMEgfs/fix/shield/gaus_N${res}.nc}
FIXWGTS=${FIXWGTS:-$FIXfv3/$CASE/fv3_SCRIP_${CASE}_GRIDSPEC_lon${LONB_SFC}_lat${LATB_SFC}.gaussian.neareststod.nc}
FIXWGTS2=${FIXWGTS2:-$FIXfv3/$CASE/fv3_SCRIP_${CASE}_GRIDSPEC_lon${LONB_SFC}_lat${LATB_SFC}.gaussian.bilinear.nc}
FIXELONELAT=${FIXELONELAT:-$FIXshield/c2g_weight_${CASE}.nc}
DATA=${DATA:-$(pwd)}

#  Filenames.
XC=${XC:-''}
GAUSFCFCSTEXE=${GAUSFCFCSTEXE:-$EXECgfs/gaussian_sfcfcst.x}
SIGLEVEL=${SIGLEVEL:-$FIXam/global_hyblev.l${LEVS}.txt}

CDATE=${CDATE:?}

#  Other variables.
export NLN=${NLN:-"/bin/ln -sf"}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export REDOUT=${REDOUT:-'1>'}
export REDERR=${REDERR:-'2>'}

# Set defaults
################################################################################
#  Preprocessing
pwd=$(pwd)
if [[ -d $DATA ]]
then
   mkdata=NO
else
   mkdir -p $DATA
   mkdata=YES
fi
cd $DATA||exit 99
mkdir -p gaussian_sfcf$( printf "%03d" $fhour)
cd gaussian_sfcf$( printf "%03d" $fhour)

################################################################################
#  Make surface forecast
export PGM=$GAUSFCFCSTEXE
export pgm=$PGM
$LOGSCRIPT

PDY=$(echo $CDATE | cut -c1-8)
cyc=$(echo $CDATE | cut -c9-10)
iy=$(echo $CDATE | cut -c1-4)
im=$(echo $CDATE | cut -c5-6)
id=$(echo $CDATE | cut -c7-8)
ih=$(echo $CDATE | cut -c9-10)

export OMP_NUM_THREADS=${OMP_NUM_THREADS_SFC:-1}

# input interpolation weights
$NLN $FIXWGTS ./weights.nc
$NLN $FIXWGTS2 ./weightb.nc
$NLN $FIXGAUS  ./gaus_N${res}.nc
$NLN $FIXELONELAT ./c2g_weight_${CASE}.nc

# input orography tiles
$NLN $FIXfv3/$CASE/${CASE}_oro_data.tile1.nc   ./orog.tile1.nc
$NLN $FIXfv3/$CASE/${CASE}_oro_data.tile2.nc   ./orog.tile2.nc
$NLN $FIXfv3/$CASE/${CASE}_oro_data.tile3.nc   ./orog.tile3.nc
$NLN $FIXfv3/$CASE/${CASE}_oro_data.tile4.nc   ./orog.tile4.nc
$NLN $FIXfv3/$CASE/${CASE}_oro_data.tile5.nc   ./orog.tile5.nc
$NLN $FIXfv3/$CASE/${CASE}_oro_data.tile6.nc   ./orog.tile6.nc

$NLN $SIGLEVEL                                 ./vcoord.txt

RSTR=${RSTR:-"3"}
RINTV=${RINTV:-"1"}
REND=${REND:-"9"}

rPDY=$(echo $RDATE | cut -c1-8)
rcyc=$(echo $RDATE | cut -c9-10)
# input forecast tiles (with nst records)
if [[ $RHR -ne $REND ]] ; then
   list1=`ls -C1 $DATA/RESTART/${rPDY}.${rcyc}0*.sfc_data.tile*.nc`
   for file in $list1; do
       $NLN $file ./fcst${file#$DATA/RESTART/${rPDY}.${rcyc}0*.sfc_data}
   done
else
   $NLN $DATA/RESTART/sfc_data.tile1.nc   ./fcst.tile1.nc
   $NLN $DATA/RESTART/sfc_data.tile2.nc   ./fcst.tile2.nc
   $NLN $DATA/RESTART/sfc_data.tile3.nc   ./fcst.tile3.nc
   $NLN $DATA/RESTART/sfc_data.tile4.nc   ./fcst.tile4.nc
   $NLN $DATA/RESTART/sfc_data.tile5.nc   ./fcst.tile5.nc
   $NLN $DATA/RESTART/sfc_data.tile6.nc   ./fcst.tile6.nc
fi

if [[ $RHR -eq 0 ]]; then
   $NLN $DATA/gfs_surface_ic*.tile1.nc   ./gfs_surface.tile1.nc
   $NLN $DATA/gfs_surface_ic*.tile2.nc   ./gfs_surface.tile2.nc
   $NLN $DATA/gfs_surface_ic*.tile3.nc   ./gfs_surface.tile3.nc
   $NLN $DATA/gfs_surface_ic*.tile4.nc   ./gfs_surface.tile4.nc
   $NLN $DATA/gfs_surface_ic*.tile5.nc   ./gfs_surface.tile5.nc
   $NLN $DATA/gfs_surface_ic*.tile6.nc   ./gfs_surface.tile6.nc
else
   $NLN $DATA/gfs_surface.tile1.nc       ./gfs_surface.tile1.nc
   $NLN $DATA/gfs_surface.tile2.nc       ./gfs_surface.tile2.nc
   $NLN $DATA/gfs_surface.tile3.nc       ./gfs_surface.tile3.nc
   $NLN $DATA/gfs_surface.tile4.nc       ./gfs_surface.tile4.nc
   $NLN $DATA/gfs_surface.tile5.nc       ./gfs_surface.tile5.nc
   $NLN $DATA/gfs_surface.tile6.nc       ./gfs_surface.tile6.nc
fi

riy=$(echo $RDATE | cut -c1-4)
rim=$(echo $RDATE | cut -c5-6)
rid=$(echo $RDATE | cut -c7-8)
rih=$(echo $RDATE | cut -c9-10)

# Namelist uses booleans now
if [[ ${DONST} == "YES" ]]; then do_nst='.true.'; else do_nst='.false.'; fi

# Executable namelist
cat <<EOF > fort.41
    &setup
     yy=$iy,
     mm=$im,
     dd=$id,
     hh=$ih,
     fhr=$fhour,
     diag_fhr=$diag_fhr,
     igaus=$LONB_SFC,
     jgaus=$LATB_SFC,
     gaus_file="gaus_N${res}"
     netcdf_out=$NETCDF_OUT
     fhzero=$FHZER
     imp_physics=11
     dtp=$DELTIM 
     donst=${do_nst}
     sfc4post=$SFC4POST
    /
EOF

# output gaussian global surface forecast files
$NLN $memdir/${APREFIX}sfcf$( printf "%03d" $fhour)${ASUFFIX} ./sfc.gaussian.nc

eval $GAUSFCFCSTEXE >> $DATA/logf$( printf "%03d" $fhour)
export ERR=$?
export err=$ERR
$ERRSCRIPT||exit 2

################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = YES ]]&&rmdir $DATA

exit $err
