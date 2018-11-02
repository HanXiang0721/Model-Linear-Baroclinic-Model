#!/bin/csh -f
#
#      sample script for BS initialization (dry model)
#
# NQS command for mail
#@$-q short
#
setenv LNHOME   /home/hiro/ln_solver                   # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYSTEM   sun                                    # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t21ml5chs     # Excutable file
setenv DIR      $LNHOME/data/out                       # Directory for Output
setenv DATZ     $LNHOME/bs/gt3/grz.t21                 # topography
#
setenv BSFILE   $LNHOME/bs/gt3/ncepwin.t21l5           # Atm. BS File (in)
setenv MODGT    $LNHOME/bs/gt3/ncepwin.t21l5.mod       # Atm. BS File (out)
setenv MODGR    $LNHOME/bs/grads/ncepwin.t21l5.mod.grd # Atm. BS File (out)
#
#setenv BSFILE   $LNHOME/bs/gt3/ncepwin.t21l5zm           # Atm. BS File (in)
#setenv MODGT    $LNHOME/bs/gt3/ncepwin.t21l5zm.mod       # Atm. BS File (out)
#setenv MODGR    $LNHOME/bs/grads/ncepwin.t21l5zm.mod.grd # Atm. BS File (out)
#
setenv EPTH     0.001
setenv TEND     30
#
#
if (! -e $DIR) mkdir -p $DIR
cd $DIR
\rm SYSOUT
echo job started at `date` > $DIR/SYSOUT
/bin/rm -f $DIR/SYSIN
#
#      parameters
#
cat << END_OF_DATA >>! $DIR/SYSIN

 &nmrun  run='nonlinear model'                              &end
 &nmtime start=0,1,1,0,0,0, end=0,1,$TEND,0,0,0             &end
 &nmdelt delt=40, tunit='MIN', inistp=0                     &end
 &nmbs    fbs='$BSFILE'                                     &end
 &nmmodbs fbsgt='$MODGT', fbsgr='$MODGR'                    &end
 &nmdata item='GRZ',    file='$DATZ'                        &end
 &nmeps  eps=$EPTH                                          &end
END_OF_DATA
#
#  run
#
( $RUN < $DIR/SYSIN >> $DIR/SYSOUT ) >& $DIR/ERROUT
echo job end at `date` >> $DIR/SYSOUT
