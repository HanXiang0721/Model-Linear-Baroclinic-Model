#!/bin/csh -f
#
#      storm track model
#      shell for Dec-alpha 
#      output is daily average
#
# NQS command for mail
#@$-q short
#
setenv LNHOME   /home/tropics/hiro/ln_solver           # ROOT of model
setenv LBMDIR   $LNHOME/model                          # model directory
setenv SYSTEM   sun                                    # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t21ml11ctintgr # Excutable file
setenv DIR      $LNHOME/data/out                       # Directory for Output
setenv TDIR     $LNHOME/solver/etc                     # tools, Gtool->GrADS
setenv BSFILE   $LNHOME/bs/gt3/ncepwin.t21l11          # Atm. BS File
setenv RSTFILE  $DIR/Restart.amat                      # Restart-Data File
setenv FRC      $LNHOME/data/frc/frc.random1000.l11.grd # initial perturbation
setenv SFRC     
setenv TRANS    $TDIR/outstr                           # executable
setenv CHECK    $TDIR/chkval                           # executable
#
# tuning parameters
#
setenv ILNGTH   6                                      # length of integration
setenv NINTG    500                                    # No. of integration
setenv NINTV    1                                      # output inteval
#
#      loop for integration
#
rm -f $DIR/SYSOUT2
echo job start at `date`  > $DIR/SYSOUT2
#
@ iintg = 1
while ( $iintg <= $NINTG )
#
if (! -e $DIR) mkdir -p $DIR
cd $DIR
rm -f SYSOUT ERROUT SETPAR
echo job started at `date` > $DIR/SYSOUT
/bin/rm -f $DIR/SYSIN
#
#      parameters
#
cat << END_OF_DATA >>! $DIR/SYSIN

 &nmrun  run='storm track model'                        &end
 &nmtime start=0,1,1,0,0,0, end=0,1,$ILNGTH,24,0,0      &end
 &nmdelt delt=40, tunit='MIN', inistp=2                 &end
 &nmhdif order=8, tefold=12, tunit='HOUR'               &end
 &nmdamp ddragv=2.,2.,2.,4.,4.,-1.,-1.,-1.,-1.,-1.,-1.,
         ddragd=2.,2.,2.,4.,4.,-1.,-1.,-1.,-1.,-1.,-1.,
         ddragt=2.,2.,2.,4.,4.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end

 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3  &end

 &nminit file='$BSFILE' , DTBFR=0., DTAFTR=0., TUNIT='DAY' &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end

 &nmchck ocheck=f, ockall=f                             &end

 &nmfrc  ffrc='$FRC',   oper=t, nfcs=$iintg             &end
 &nmsfrc fsfrc='$SFRC', ofrc=f, nsfcs=1                 &end

 &nmdata item='GRZ',    file=''                         &end

 &nmhisd tintv=1, tavrg=3, tunit='HOUR'                 &end
 &nmhist item='PSI',  file='s',    tintv=$NINTV, tavrg=$NINTV, tunit='DAY' &end
 &nmhist item='VOR',  file='r',    tintv=$NINTV, tavrg=$NINTV, tunit='DAY' &end
 &nmhist item='U',    file='u',    tintv=$NINTV, tavrg=$NINTV, tunit='DAY' &end
 &nmhist item='V',    file='v',    tintv=$NINTV, tavrg=$NINTV, tunit='DAY' &end
 &nmhist item='T',    file='t',    tintv=$NINTV, tavrg=$NINTV, tunit='DAY' &end
 &nmhist item='Z',    file='z',    tintv=$NINTV, tavrg=$NINTV, tunit='DAY' &end
 &nmhist item='PS',   file='p',    tintv=$NINTV, tavrg=$NINTV, tunit='DAY' &end
END_OF_DATA
#
#  run
#
( $RUN < $DIR/SYSIN >> $DIR/SYSOUT ) >& $DIR/ERROUT
echo job end at `date` >> $DIR/SYSOUT
#
#      parameter for trans
#
cat << END_TRANS >>! $DIR/SETPAR
 &nmrec nfcs=$iintg, nfday=2, nlday=$ILNGTH, tint=$NINTV, tunit='DAY' &end
 &nmfili cfs='$DIR/s',
         cfr='$DIR/r',
         cfu='$DIR/u',
         cfv='$DIR/v',
         cft='$DIR/t',
         cfz='$DIR/z',
         cfp='$DIR/p',
 &end
 &nmfilo cfo='$LNHOME/data/strack/out1000.grd',
         cfo2='$LNHOME/data/strack/eddy.stm.grd',
         cbs='$LNHOME/bs/grads/ncepwin.t21l11.grd',
         ofo=f
 &end
END_TRANS
$TRANS < $DIR/SETPAR >> $DIR/SYSOUT2
#
   @ iintg++ 
end
#
#      parameter for check
#
rm -f $DIR/SETPAR
cd $DIR
cat << END_CHECK >>! $DIR/SETPAR
 &nmrec nfcs=$NINTG, nfday=2, nlday=$ILNGTH, tint=$NINTV, tunit='DAY'  &end
 &nmfil cfo='$LNHOME/data/strack/out1000.grd',
        cfoc='$LNHOME/data/strack/out1000.c.grd',
 &end
END_CHECK
$CHECK < $DIR/SETPAR >> $DIR/SYSOUT2
echo job end at `date` >> $DIR/SYSOUT2

