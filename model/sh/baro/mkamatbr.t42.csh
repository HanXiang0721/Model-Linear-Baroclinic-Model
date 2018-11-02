#! /usr/bin/tcsh -f
#
#      making linear barotropic matrix 
#      no need for post-process
#
# NQS command for mail
#@$-q short
#
setenv LNHOME   /home/hiro/ln_solver                   # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYSTEM   sun                                    # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t42ml1cbaro   # Excutable file
setenv DIR      $LNHOME/matrix                         # Output Dir
##setenv BSFILE   $LNHOME/bs/gt3/ncepfeb.t42l1           # Atm. BS File
##setenv BSFILE   $LNHOME/bs/gt3/ncepdec.t42l1           # Atm. BS File
setenv BSFILE   $LNHOME/bs/gt3/ncepfeb.t42l1zm         # Atm. BS File
setenv MATDIR   $DIR/mat                               # matrix directory
##setenv MATNAM   MATBR.ncepfeb.dat                      # matrix file name
##setenv MATNAM   MATBR.ncepdec.dat                      # matrix file name
setenv MATNAM   MATBR.ncepfebzm.dat                    # matrix file name
#
# T42 L1
#
setenv NLEVS     1  # no. of vertical levels
setenv NWSTR     0   # first zonal wave number
setenv NWEND    42   # truncate wave number
setenv KORD      4
setenv KTEF      1
#
setenv STM       1
setenv STD       1
#
/bin/rm -f $DIR/out/SYSOUT* $DIR/out/ERROUT
/bin/rm -f $MATDIR/$MATNAM
echo --- exec error messages --- > $DIR/out/ERROUT
#
#       resolution	
#
@ maxw = $NWEND
@ izwv = $NWSTR
while ( $izwv <= $maxw ) 
#
#	loop for a unity (NMDIM) of vor.
#
echo VOR zonal wave M is $izwv
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $DIR/out) mkdir -p $DIR/out
if (! -e $MATDIR) mkdir -p $MATDIR
cd $DIR
echo job started at `date` > $DIR/out/SYSOUT$izwv
/bin/rm -f $DIR/out/SYSIN
#
#      parameters
#
cat << END_OF_DATA >>! $DIR/out/SYSIN

 &nmrun  run='make matrix'                                     &end
 &nmtime start=0,$STM,$STD,0,0,0, end=0,$STM,$STD,0,30,0       &end
 &nmdelt delt=40, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='DAY'                &end
 &nmdamp nddragv=10,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=10.                                            &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'    &end

 &nmchck ocheck=t, ockall=f                                    &end

 &nmamat amatf='$MATDIR/$MATNAM', kmatw=$izwv                  &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
echo job end at `date` >> $DIR/out/SYSOUT$izwv
#
   @ izwv++
end
#
echo redist end at `date` 
