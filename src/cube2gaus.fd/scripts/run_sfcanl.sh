#!/bin/ksh

set -x

SDATE=2021052800
EDATE=2021060418
EDATE=$SDATE
dhour=6
CASE=C384
#CASE=C192
#CASE=C768
NETCDF_OUT=.true.
FHZER=6
DELTIM=240
IAU_delta=6
RESTART="YES"

exp=rpgcycletref_nocean
DONST="NO"
NDATE=/scratch2/NCEPDEV/nwprod/NCEPLIBS/utils/prod_util.v1.1.0/exec/ndate
HOMEgfs=/scratch2/GFDL/gfdlscr/Mingjing.Tong/global_workflow/shield_develop

if [ ! -d /scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout ]; then
  mkdir -p /scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout
fi

CDATE=$SDATE
while [[ $CDATE -le $EDATE ]]; do 
  ymd=`echo $CDATE | cut -c1-8`
  hh=`echo $CDATE | cut -c9-10`
  DATA=/scratch2/NCEPDEV/stmp1/Mingjing.Tong/c2g_sfcanl/$CDATE
  GDATE=$CDATE
  if [ $RESTART = "YES" ]; then
    #COMOUT=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/ICS/gdas.${ymd}/${hh}/atmos/RESTART
    #COMOUT=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/ICS/gdas.${ymd}/${hh}/atmos/RESTART_GFS
    #COMOUT=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/${exp}/enkfgdas.${ymd}/${hh}/atmos/mem001/RESTART
    COMOUT=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/${exp}/gdas.${ymd}/${hh}/atmos/RESTART
    GDATE=`$NDATE -3 $CDATE`
    ymd=`echo $GDATE | cut -c1-8`
    hh=`echo $GDATE | cut -c9-10`
  else
    COMOUT=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/ICS/gdas.${ymd}/${hh}/atmos/INPUT
    COMOUT=/scratch2/NCEPDEV/stmp1/Mingjing.Tong/RUNDIRS/${exp}/${CDATE}/gdas/fcst.128652/INPUT
  fi
  GAUSFCFCSTEXE=$HOMEgfs/exec/gaussian_sfcanl.exe
  GAUSFCSCRIPT=$HOMEgfs/sorc/cube2gaus.fd/scripts/gaussian_sfcanl.sh

  #mv $COMOUT/sfcanl/gdas.${hh}.sfcanl.nc $COMOUT/sfcanl/gdas.t${hh}z.sfcanl.nc

  if [ ! -s $COMOUT/sfcanl/gdas.t${hh}z.sfcanl.nc ]; then

  sbatch --export=HOMEgfs=$HOMEgfs,exp=$exp,DATA=$DATA,CASE=$CASE,LEVS=$LEVS,RHR=$fhr,CDATE=$GDATE,COMOUT=$COMOUT,LONB_SFC=$LONB_SFC,LATB_SFC=$LATB_SFC,NETCDF_OUT=$NETCDF_OUT,APREFIX="gdas.t${hh}z.",ASUFFIX=".nc",GAUSFCFCSTEXE=$GAUSFCFCSTEXE,DONST=$DONST --output=/scratch2/GFDL/gfdlscr/Mingjing.Tong/scrub/stdout/%x.o%j $GAUSFCSCRIPT

  fi

  CDATE=`$NDATE +$dhour $CDATE`
done

exit

