#!/bin/ksh

set -x

rmhydro=".true."
pseudo_ps=".true."
phy_data="phy_data"
CDATE=2022060918
DELTIM=450
iau_halfdelthrs=0
exp=ensreplay_test
HOMEgfs=/scratch2/GFDL/gfdlscr/Mingjing.Tong/global_workflow/shield_intermediate
DATA=/scratch2/NCEPDEV/stmp1/Mingjing.Tong/RUNDIRS/s2022_C192_v2p5_edmf0/fcst.3277299
COMOUT=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout
USHDIR=$(pwd)
NDATE=/home/Mingjing.Tong/bin/ndate

rm -rf $COMOUT/*

#for fhr in 3 4 5 6 7 8 9
for fhr in 9
do

  RHR=$fhr

  if [[ $RHR == 0 ]]; then
    export fhour=$( echo "$DELTIM / 3600" | bc)
  else
    export fhour=$(( $RHR-$iau_halfdelthrs ))
  fi

  RDATE=$($NDATE +$fhr $CDATE)
  sbatch --export=HOMEgfs=$HOMEgfs,DATA=$DATA,exp=$exp,atminc=".false.",rmhydro=$rmhydro,pseudo_ps=$pseudo_ps,phy_data=$phy_data,CASE=C192,LEVS=91,RHR=$fhr,fhour=$fhour,CDATE=$CDATE,RDATE=$RDATE,COMOUT=$COMOUT, --output=$COMOUT/%x.o%j $USHDIR/gaussian_c2g_atms.sh
done
