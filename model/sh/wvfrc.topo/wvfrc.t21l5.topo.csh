#! /usr/bin/tcsh -f
#
#      making zonal-wave coupling term
#
# NQS command for mail
#@$-q short
#
setenv LNHOME   /home/hiro/ln_solver                  # ROOT of model
setenv LBMDIR   $LNHOME/model                         # ROOT of LBM 
setenv SYSTEM   sun                                   # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t21ml5cwvfrc.topo # Exec file
setenv DATZ     $LNHOME/bs/gt3/grz.t21                # Topography Data
setenv DIR      $LNHOME/data                          # Output dir
setenv BSFILE   $LNHOME/bs/gt3/ncepwin.t21l5          # Atm. BS File
setenv RSTFILE  $DIR/out/Restart.amat                 # Restart file
#
# output
setenv FRCGRD   $LNHOME/data/frc/frc.t21l5zm.topo.grd # topo. forcing
#
# T21L5 
#
setenv STM       1
setenv STD       1
#
/bin/rm -f $DIR/out/SYSOUT $DIR/out/ERROUT
echo --- exec error messages --- > $DIR/out/ERROUT
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $DIR/out) mkdir -p $DIR/out
cd $DIR
echo job started at `date` > $DIR/out/SYSOUT
/bin/rm -f $DIR/out/SYSIN
#
#      parameters
#
cat << END_OF_DATA >>! $DIR/out/SYSIN

 &nmrun  run='make frcing'                                     &end
 &nmtime start=0,$STM,$STD,0,0,0, end=0,$STM,$STD,0,30,0       &end
 &nmdelt delt=40, tunit='MIN', inistp=0                        &end
 &nmhdif order=4, tefold=1, tunit='DAY', oraycl=f, ray0=100000, raycl0=5  &end
 &nminit file='$BSFILE' ,   DTBFR=1., DTAFTR=1., TUNIT='YEAR'  &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0                                              &end
 &nmvdif vdifv=-1.,-1.,-1.,-1.,-1.,
         vdifd=-1.,-1.,-1.,-1.,-1.,
         vdift=-1.,-1.,-1.,-1.,-1.
 &end

 &nmwfrc wvfrc='  '       &end 
 &nmamat amatf='$FRCGRD'  &end 

 &nmdata item='GRZ',    file='$DATZ'            &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT ) >& $DIR/out/ERROUT
echo job end at `date` >> $DIR/out/SYSOUT
echo end at `date` 
