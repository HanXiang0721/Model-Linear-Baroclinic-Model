#!/bin/csh -f
#
#      making forcing vector for moist LBM
#
# NQS command for mail
#@$-q short
#
setenv LNHOME   /home/tropics/hiro/ln_solver           # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYSTEM   sun                                    # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t21ml8cmkamat # Excutable file
setenv DIR      $LNHOME/data                           # Directory for Output
setenv RSTFILE  $DIR/out/Restart.amat                  # Restart-Data File
#
# basic states, winter 3D
setenv DATZ     $LNHOME/bs/gt3/grz.t21                 # Topography
setenv DATS     $LNHOME/bs/gt3/sstwin                  # mean SST
setenv DATW     $LNHOME/bs/gt3/wgwin                   # soil wetness
setenv DATI     $LNHOME/bs/gt3/gridx.t21               # sfc. index
setenv BSFILE   $LNHOME/bs/gt3/ncepwin.t21l8           # Atm. BS File
#
# SST forcing (input)
setenv FRCGRD   $DIR/frc/frcsst.t21.grd
#
# perturbation (input)
setenv ANMGRD   $DIR/rsp/rsp.t21l8.s.grd
#
# forcing matrix (output)
setenv FRCMAT   $DIR/frc/frc.t21l8.mat
#
# total heating/moistening (output) # optional
setenv TFRC     $DIR/frc/heat.t21l8.grd
#
# parameters
setenv NTRN      5   # truncate wave number
setenv STM       1
setenv STD       1
#
/bin/rm -f $DIR/out/SYSOUT* $DIR/out/ERROUT $TFRC
echo --- exec error messages --- > $DIR/out/ERROUT
#
#      setup
#
echo compute forcing vector
if (! -e $DIR) mkdir -p $DIR
if (! -e $DIR/out) mkdir -p $DIR/out
cd $DIR
echo job started at `date` > $DIR/out/SYSOUT
/bin/rm -f $DIR/out/SYSIN
#
#      parameters
#
# Use the following parameters for computing heating response 
# &nmwfrc wvfrc='$ANMGRD'                                       &end
# &nmsfrc fsfrc='$FRCGRD', gsfrc='$TFRC'                        &end
#
cat << END_OF_DATA >>! $DIR/out/SYSIN
 &nmrun  run='make forcing'                                    &end
 &nmtime start=0,$STM,$STD,0,0,0, end=0,$STM,$STD,0,30,0       &end
 &nmdelt delt=40, tunit='MIN', inistp=0                        &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'    &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON'                 &end
 &nmmca ttau0=3, qtau0=3, ttauc=6, tunit='HOUR', sigkb=1.D0    &end
 &nmsfcm owes=t, expw=0.5                                      &end
 &nmsfrc fsfrc='$FRCGRD'                                       &end
 &nmamat amatf='$FRCMAT', 
  omatv=t, omatd=t, omatt=t, omatp=t, omatq=t, kmatw=$NTRN     &end
 &nmdata item='GRZ',    file='$DATZ'                           &end
 &nmdata item='GRSST',  file='$DATS'                           &end
 &nmdata item='GRIDX',  file='$DATI'                           &end
 &nmdata item='GRWG',   file='$DATW'                           &end
END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT ) >& $DIR/out/ERROUT
echo job end at `date` >> $DIR/out/SYSOUT
