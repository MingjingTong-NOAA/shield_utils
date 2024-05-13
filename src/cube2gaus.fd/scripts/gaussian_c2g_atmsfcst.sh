#!/bin/ksh
#SBATCH --job-name=gaussian_c2g_atmfcst
#SBATCH --output=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout/%x.o%j
#SBATCH --qos=batch
#SBATCH --account=gfdlhires
#SBATCH --time=0:10:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=40
#SBATCH --export=res=384,ALL

# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

#  Set environment.
VERBOSE=${VERBOSE:-"YES"}
if [[ $atminc = ".true." ]]; then
   logfile="logatminc"
else
   logfile="logf$( printf "%03d" $fhour)"
fi
if [[ "$VERBOSE" = "YES" ]] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

CASE=C${res}

#  Directories.
gfs_ver=${gfs_ver:-v15.0.0}
BASEDIR=${BASEDIR:-${NWROOT:-/nwprod2}}
HOMEgfs=${HOMEgfs:-$BASEDIR/gfs_ver.${gfs_ver}}
EXECgfs=${EXECgfs:-$HOMEgfs/exec}
FIXfv3=${FIXfv3:-$HOMEgfs/fix/fix_fv3_gmted2010}
FIXam=${FIXam:-$HOMEgfs/fix/fix_am}
FIXC2G=${FIXC2G:-$HOMEgfs/fix/fix_shield/gaus_N${res}.nc}
DATA=${DATA:-$(pwd)}
COMOUT=${COMOUT:-$(pwd)}

#  Filenames.
XC=${XC}
GAUATMSEXE=${GAUATMSEXE:-$EXECgfs/gaussian_c2g_atms.x}

CDATE=${CDATE:?}

#  Other variables.
export NLN=${NLN:-"/bin/ln -sf"}
export NCP=${NCP:-"/bin/cp -p"}
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
if [[ $atminc = ".true." ]]; then
  workdir="gaussian_c2g_atminc"
else
  workdir="gaussian_c2g_atmsf$( printf "%03d" $fhour)"
fi
if [[ -d $workdir ]]; then
  rm -rf $workdir
fi
mkdir -p $workdir
cd $workdir

################################################################################
#  Make forecast file on gaussian grid
export PGM=$GAUATMSEXE
export pgm=$PGM
$LOGSCRIPT

$NCP $GAUATMSEXE ./

export OMP_NUM_THREADS=${OMP_NUM_THREADS_ATMS:-40}

REND=${REND:-"9"}

yyyy=$(echo $CDATE | cut -c1-4)
mm=$(echo $CDATE | cut -c5-6)
dd=$(echo $CDATE | cut -c7-8)
hh=$(echo $CDATE | cut -c9-10)
APREFIX="gdas.t${hh}z."
OUTPUT_FILE=${OUTPUT_FILE:-"netcdf"}
if [ $OUTPUT_FILE = "netcdf" ]; then
   nemsio=".false."
   ASUFFIX=".nc"
else
   nemsio=".true."
   ASUFFIX=".nemsio"
fi

# input interpolation weights
$NLN $FIXC2G ./gaus_N${res}.nc

$NLN $DATA/grid_spec.tile1.nc ./grid_spec.tile1.nc
$NLN $DATA/grid_spec.tile2.nc ./grid_spec.tile2.nc
$NLN $DATA/grid_spec.tile3.nc ./grid_spec.tile3.nc
$NLN $DATA/grid_spec.tile4.nc ./grid_spec.tile4.nc
$NLN $DATA/grid_spec.tile5.nc ./grid_spec.tile5.nc
$NLN $DATA/grid_spec.tile6.nc ./grid_spec.tile6.nc
$NLN $DATA/control.dat ./control.dat

if [[ $get_weights_only = ".false." ]] ; then
if [[ $atminc = ".false." ]]; then
  rPDY=$(echo $RDATE | cut -c1-8)
  rcyc=$(echo $RDATE | cut -c9-10)
  if [[ $RHR -ne $REND ]] ; then
     list1=`ls -C1 $DATA/RESTART/${rPDY}.${rcyc}0*.fv_core.res.*`
     list2=`ls -C1 $DATA/RESTART/${rPDY}.${rcyc}0*.fv_tracer.res.*`
     list3=`ls -C1 $DATA/RESTART/${rPDY}.${rcyc}0*.phy_data.*`
     list4=`ls -C1 $DATA/RESTART/${rPDY}.${rcyc}0*.coupler.res`	
  else
     list1=`ls -C1 $DATA/RESTART/fv_core.res.*`
     list2=`ls -C1 $DATA/RESTART/fv_tracer.res.*`
     list3=`ls -C1 $DATA/RESTART/phy_data.*`
     list4=`ls -C1 $DATA/RESTART/coupler.res`
  fi
  for list in $list1 $list2 $list3; do
      for file in $list; do
         if [[ $RHR -ne $REND ]] ; then
            $NLN $file ./${file#$DATA/RESTART/${rPDY}.${rcyc}0*.}
         else
            $NLN $file ./${file#$DATA/RESTART/}
         fi
      done
  done  
  data_out="atmf$( printf "%03d" $fhour)${ASUFFIX}"  
else
  list1=`ls -C1 $DATA/ATMINC/atminc.fv_core.res.*`
  list2=`ls -C1 $DATA/ATMINC/atminc.fv_tracer.res.*`
  for list in $list1 $list2 $list3; do
      for file in $list; do
         $NLN $file ./${file#$DATA/RESTART/atminc.}
      done
  done
  data_out="atminc"
fi

# output gaussian global forecast files
$NLN $memdir/${APREFIX}${data_out}${ASUFFIX} ./$data_out
fi

# Executable namelist
cat > fv3_da.nml <<EOF
   &fv3_da_nml
    finer_steps = 0,
    nvar3dout = 14,
    write_res = .true.,
    read_res = .true.,
    write_nemsio = $nemsio,
    rmhydro = ${rmhydro},
    pseudo_ps = ${pseudo_ps},
    atminc = ${atminc},
    data_file(1) = "fv_tracer.res",
    data_file(2) = "fv_core.res",
    data_file(3) = "${phy_data}",
    data_out = ${data_out},
    gaus_file = "gaus_N${res}",
    atmos_nthreads = $OMP_NUM_THREADS,
    yy=$yyyy,
    mm=$mm,
    dd=$dd,
    hh=$hh,
    fhr=$fhour,
    ideflate=1,
    nbits=14,
    get_weights_only=${get_weights_only},
/
EOF

eval $GAUATMSEXE > $logfile
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
