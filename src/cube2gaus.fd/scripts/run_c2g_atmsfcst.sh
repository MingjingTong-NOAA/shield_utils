#!/bin/ksh

set -x

model="gfs"
rmhydro=".true."
pseudo_ps=".true."
atminc=".false."
phy_data=""
get_weights_only=".false."
res=384
#res=192
LEVS=91
CDATE=2021060218
VERBOSE="YES"
exp=test
HOMEgfs=/scratch2/GFDL/gfdlscr/Mingjing.Tong/global_workflow/shield_develop
DATA=/scratch1/NCEPDEV/stmp2/Mingjing.Tong/RUNDIRS/smptref/2021060218/gfs/fcst.239637
if [[ $get_weights_only = ".true." ]]; then
  DATA=/scratch2/GFDL/gfdlscr/Mingjing.Tong/fix_shield/grid_file/C$res
  exp=c2g_weights
fi
COMOUT=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/c2g_atmfcst/$CDATE/$exp
NDATE=/scratch2/NCEPDEV/nwprod/NCEPLIBS/utils/prod_util.v1.1.0/exec/ndate

if [ ! -d /scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout ]; then
  mkdir -p /scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout
fi

if [ ! -d $COMOUT ]; then
  mkdir -p $COMOUT
fi

for fhr in 6
do
  fhour=$((1.0*fhr))
  RDATE=$($NDATE +$fhr $CDATE)
  sbatch --export=HOMEgfs=$HOMEgfs,DATA=$DATA,model=$model,exp=$exp,rmhydro=$rmhydro,pseudo_ps=$pseudo_ps,phy_data=$phy_data,res=$res,LEVS=$LEVS,RHR=$fhr,fhour=$fhour,CDATE=$CDATE,RDATE=$RDATE,COMOUT=$COMOUT,get_weights_only=$get_weights_only,atminc=$atminc,VERBOSE=$VERBOSE --output=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout/%x.o%j ./gaussian_c2g_atms.sh
done

exit

