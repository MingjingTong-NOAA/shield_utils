#!/bin/ksh

set -x

res=384
#res=192
LEVS=91
exp=rp_smptref_tnocean_gcycle2
CDATE=2021052721
ymd=`echo $CDATE | cut -c1-8`
hh=`echo $CDATE | cut -c9-10`
NETCDF_OUT=.true.
FHZER=0
DELTIM=240

HOMEgfs=/scratch2/GFDL/gfdlscr/Mingjing.Tong/global_workflow/shield_develop
#DATA=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/${exp}/enkfgdas.${ymd}/${hh}/atmos/mem051
DATA=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/${exp}/gdas.${ymd}/${hh}/atmos
DATA=/scratch2/NCEPDEV/stmp1/Mingjing.Tong/RUNDIRS/rpgcycletref/2021052800/gdas/fcst.253763
NDATE=/scratch2/NCEPDEV/nwprod/NCEPLIBS/utils/prod_util.v1.1.0/exec/ndate
GAUSFCFCSTEXE=$HOMEgfs/exec/gaussian_sfcfcst.exe
GAUSFCSCRIPT=./gaussian_sfcfcst.sh

export DONST="NO"
export SFC4POST=.false.

#COMOUT=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/c2g_sfcfcst/$exp/$CDATE
COMOUT=$DATA

if [ ! -d /scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout ]; then
  mkdir -p /scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout
fi

if [ ! -d $COMOUT ]; then
  mkdir -p $COMOUT
fi

iau_halfdelthrs=$((IAU_DELTHRS/2))

#for fhr in 3 4 5 6 7 8 9
#for fhr in 3 6
for fhr in 0
do
  RHR=$fhr
  RDATE=$($NDATE +$fhr $CDATE)
  fhour=$((1.0*(RHR-iau_halfdelthrs)))
  diag_fhr=$((fhour+2*iau_halfdelthrs))
  sbatch --export=HOMEgfs=$HOMEgfs,exp=$exp,DATA=$DATA,res=$res,LEVS=$LEVS,RHR=$RHR,CDATE=$CDATE,RDATE=$RDATE,COMOUT=$COMOUT,LONB_SFC=$LONB_SFC,LATB_SFC=$LATB_SFC,NETCDF_OUT=$NETCDF_OUT,FHZER=$FHZER,fhour=$fhr,diag_fhr=$diag_fhr,imp_physics=11,DONST=$DONST,SFC4POST=$SFC4POST,DELTIM=$DELTIM,GAUSFCFCSTEXE=$GAUSFCFCSTEXE --output=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout/%x.o%j $GAUSFCSCRIPT
done

exit

