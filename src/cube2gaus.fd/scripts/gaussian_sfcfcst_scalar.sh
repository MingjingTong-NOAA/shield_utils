#!/bin/ksh
#SBATCH --job-name=gaussian_c2g_sfcfcst
#SBATCH --output=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout/%x.o%j
#SBATCH --qos=batch
#SBATCH --account=gfdlhires
#SBATCH --time=0:10:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --export=res=384,ALL

# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

#  Set environment.
VERBOSE=${VERBOSE:-"YES"}
if [[ "$VERBOSE" = "YES" ]] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

CASE=C${res}
res=$(echo $CASE | cut -c2-)
LONB_CASE=$((res*4))
LATB_CASE=$((res*2))
LONB_SFC=${LONB_SFC:-$LONB_CASE}
LATB_SFC=${LATB_SFC:-$LATB_CASE}
DONST=${DONST:-"NO"}
LEVS=${LEVS:-91}
OUTPUT_FILE=${OUTPUT_FILE:-"netcdf"}
if [ $OUTPUT_FILE = "netcdf" ]; then
    export NETCDF_OUT=".true."
    ASUFFIX=".nc"
else
    export NETCDF_OUT=".false."
    ASUFFIX=".nemsio"
fi

#  Directories.
gfs_ver=${gfs_ver:-v15.0.0}
BASEDIR=${BASEDIR:-${NWROOT:-/nwprod2}}
HOMEgfs=${HOMEgfs:-$BASEDIR/gfs_ver.${gfs_ver}}
EXECgfs=${EXECgfs:-$HOMEgfs/exec}
FIXfv3=${FIXfv3:-$HOMEgfs/fix/fix_fv3_gmted2010}
FIXshield=${FIXshield:-$HOMEgfs/fix/fix_shield}
FIXGAUS=${FIXGAUS:-$HOMEgfs/fix/fix_shield/gaus_N${res}.nc}
FIXam=${FIXam:-$HOMEgfs/fix/fix_am}
FIXWGTS=${FIXWGTS:-$FIXfv3/$CASE/fv3_SCRIP_${CASE}_GRIDSPEC_lon${LONB_SFC}_lat${LATB_SFC}.gaussian.neareststod.nc}
FIXWGTS2=${FIXWGTS2:-$FIXfv3/$CASE/fv3_SCRIP_${CASE}_GRIDSPEC_lon${LONB_SFC}_lat${LATB_SFC}.gaussian.bilinear.nc}
FIXELONELAT=${FIXELONELAT:-$FIXshield/c2g_elonelat_${CASE}.nc}
DATA=${DATA:-$(pwd)}

#  Filenames.
XC=${XC}
GAUSFCFCSTEXE=${GAUSFCFCSTEXE:-$EXECgfs/gaussian_sfcfcst.exe}
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
$INISCRIPT
pwd=$(pwd)
if [[ -d $DATA ]]
then
   mkdata=NO
else
   mkdir -p $DATA
   mkdata=YES
fi
cd $DATA||exit 99
[[ -d $COMOUT ]]||mkdir -p $COMOUT
if [[ -d gaussian_c2g_sfcf$( printf "%03d" $RHR)_${exp} ]]; then
   rm -rf gaussian_c2g_sfcf$( printf "%03d" $RHR)_${exp}
fi
mkdir -p gaussian_c2g_sfcf$( printf "%03d" $RHR)_${exp}
cd gaussian_c2g_sfcf$( printf "%03d" $RHR)_${exp}

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
$NLN $FIXWGTS  ./weights.nc
$NLN $FIXWGTS2 ./weightb.nc
$NLN $FIXGAUS  ./gaus_N${res}.nc

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
   $NLN $DATA/RESTART/${rPDY}.${rcyc}0*.sfc_data.tile1.nc   ./fcst.tile1.nc
   $NLN $DATA/RESTART/${rPDY}.${rcyc}0*.sfc_data.tile2.nc   ./fcst.tile2.nc
   $NLN $DATA/RESTART/${rPDY}.${rcyc}0*.sfc_data.tile3.nc   ./fcst.tile3.nc
   $NLN $DATA/RESTART/${rPDY}.${rcyc}0*.sfc_data.tile4.nc   ./fcst.tile4.nc
   $NLN $DATA/RESTART/${rPDY}.${rcyc}0*.sfc_data.tile5.nc   ./fcst.tile5.nc
   $NLN $DATA/RESTART/${rPDY}.${rcyc}0*.sfc_data.tile6.nc   ./fcst.tile6.nc
else
   $NLN $DATA/RESTART/sfc_data.tile1.nc   ./fcst.tile1.nc
   $NLN $DATA/RESTART/sfc_data.tile2.nc   ./fcst.tile2.nc
   $NLN $DATA/RESTART/sfc_data.tile3.nc   ./fcst.tile3.nc
   $NLN $DATA/RESTART/sfc_data.tile4.nc   ./fcst.tile4.nc
   $NLN $DATA/RESTART/sfc_data.tile5.nc   ./fcst.tile5.nc
   $NLN $DATA/RESTART/sfc_data.tile6.nc   ./fcst.tile6.nc
fi

#$NLN $DATA/gfs_surface.tile*.nc   ./
$NLN $DATA/fv3_history2d.tile1.nc   ./gfs_surface.tile1.nc
$NLN $DATA/fv3_history2d.tile2.nc   ./gfs_surface.tile2.nc
$NLN $DATA/fv3_history2d.tile3.nc   ./gfs_surface.tile3.nc
$NLN $DATA/fv3_history2d.tile4.nc   ./gfs_surface.tile4.nc
$NLN $DATA/fv3_history2d.tile5.nc   ./gfs_surface.tile5.nc
$NLN $DATA/fv3_history2d.tile6.nc   ./gfs_surface.tile6.nc

# output gaussian global surface forecast files
APREFIX="gdas.t${ih}z."
$NLN $COMOUT/${APREFIX}sfcf$( printf "%03d" $fhour)${ASUFFIX} ./sfc.gaussian.nc

riy=$(echo $RDATE | cut -c1-4)
rim=$(echo $RDATE | cut -c5-6)
rid=$(echo $RDATE | cut -c7-8)
rih=$(echo $RDATE | cut -c9-10)

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
    /
EOF

$GAUSFCFCSTEXE > gaussian_sfcf$( printf "%03d" $RHR).log
export ERR=$?
export err=$ERR
$ERRSCRIPT||exit 2

################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = YES ]]&&rmdir $DATA
$ENDSCRIPT
set +x
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
