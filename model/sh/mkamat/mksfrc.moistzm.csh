#!/bin/csh -f
#
#      making forcing vector for moist LBM
#
# NQS command for mail
#@$-q short
#
setenv LNHOME   /home/hiro/ln_solver                   # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYSTEM   sun                                    # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t21ml11cmkamat # Excutable file
setenv DIR      $LNHOME/data                           # Directory for Output
setenv RSTFILE  $DIR/out/Restart.amat                  # Restart-Data File
#
# basic states, winter sym. ZM
#setenv DATS     $LNHOME/bs/gt3/sstwin.zm.sy             # mean SST
#setenv BSFILE $LNHOME/bs/gt3/ncepwin.t21l11zm.sy        # Atm. BS File
#
# basic states, annual sym. ZM
setenv DATS     $LNHOME/bs/gt3/sstann.zm.sy             # mean SST
setenv BSFILE $LNHOME/bs/gt3/ncepann.t21l11zm.sy        # Atm. BS File
#
# SST forcing (input)
#setenv FRCGRD   $DIR/frc/frc.sst.wn1.grd
#setenv FRCGRD   $DIR/frc/frc.sst.coswn1.grd
setenv FRCGRD   $DIR/frc/frc.sst.coswn2.grd
#
# perturbation (input)
#setenv ANMGRD   $DIR/rsp/rsp.t21l8.s.grd
#
# forcing matrix (output)
#setenv FRCMAT   $DIR/frc/frc.sst.wn1.mat
#setenv FRCMAT   $DIR/frc/frc.sst.coswn1.mat
setenv FRCMAT   $DIR/frc/frc.sst.coswn2.mat
#
# total heating/moistening (output) # optional
setenv TFRC     $DIR/frc/heat.t21l8.grd
#
# parameters
setenv NTRN      6   # truncate wave number
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
 &nmmca ttau0=3, qtau0=3, ttauc=5, tunit='HOUR', sigkb=0.95D0  &end
 &nmsfcm owes=t, expw=0.5                                      &end
 &nmsfrc fsfrc='$FRCGRD'                                       &end
 &nmamat amatf='$FRCMAT', 
  omatv=t, omatd=t, omatt=t, omatp=t, omatq=t, kmatw=$NTRN     &end
 &nmdata item='GRSST',  file='$DATS'                           &end
END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT ) >& $DIR/out/ERROUT
echo job end at `date` >> $DIR/out/SYSOUT
