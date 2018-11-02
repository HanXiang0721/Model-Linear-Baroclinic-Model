#!/bin/csh -f
#
#      sample script for linear model run (dry model)
#
# NQS command for mail
#@$-q   b
#@$-N   1
#@$-me
#
setenv LNHOME   /home/q16655/ln_solver                 # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYSTEM   sr8000                                 # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t42ml20ctintgr # Excutable file
setenv TDIR     $LNHOME/solver/util
setenv FDIR     $LNHOME/data/aim/rspbc                 # Directory for Output
setenv DIR      $LNHOME/data/out                       # Directory for Output
setenv BSFILE   $LNHOME/bs/gt3/erawin.t42l20           # Atm. BS File
setenv RSTFILE  $DIR/Restart.amat                      # Restart-Data File
setenv FRC      $FDIR/frc.t42l20.tst.grd               # initial perturbation
setenv SFRC     $FDIR/frc.t42l20.tst.grd               # steady forcing
setenv TRANS    gt2gr
setenv TEND     201
#
#
if (! -e $DIR) mkdir -p $DIR
cd $DIR
#\rm SYSOUT
echo job started at `date` > $DIR/SYSOUT
/bin/rm -f $DIR/SYSIN
#
#      parameters
#
# &nmdamp ddragv=0.5,0.5,0.5,1,5,20,20,20,20,20,20,20,20,20,20,20,20,20,0.5,0.5,
#         ddragd=0.5,0.5,0.5,1,5,20,20,20,20,20,20,20,20,20,20,20,20,20,0.5,0.5,
#         ddragt=0.5,0.5,0.5,1,5,20,20,20,20,20,20,20,20,20,20,20,20,20,0.5,0.5,
#
cat << END_OF_DATA >>! $DIR/SYSIN
 &nmrun  run='linear model'                                 &end
 &nmtime start=1,1,1,0,0,0, end=1,1,$TEND,0,0,0             &end
 &nmhdif order=4, tefold=0.0833, tunit='DAY'                &end
 &nmdelt delt=20, tunit='MIN', inistp=2                     &end
 &nmdamp ddragv=0.5,0.5,0.5,5,30,30,30,30,30,30,30,30,30,30,30,30,30,30,1,0.5,
         ddragd=0.5,0.5,0.5,5,30,30,30,30,30,30,30,30,30,30,30,30,30,30,1,0.5,
         ddragt=0.5,0.5,0.5,5,30,30,30,30,30,30,30,30,30,30,30,30,30,30,1,0.5,
         tunit='DAY'                                           &end
 &nminit file='$BSFILE' , DTBFR=0., DTAFTR=0., TUNIT='DAY'   &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON',  overwt=t    &end

 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,                         &end
 &nmbtdif tdmpc=0.                                           &end
 &nmfrc  ffrc='$FRC',   oper=f, nfcs=1                       &end
 &nmsfrc fsfrc='$SFRC', ofrc=t, nsfcs=1, fsend=-1,1,30,0,0,0 &end

 &nmchck ocheck=f, ockall=f                                  &end
 &nmdata item='GRZ',    file=' '                             &end

 &nmhisd tintv=1, tavrg=1, tunit='DAY'                       &end
 &nmhist item='PSI',  file='psi', tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='CHI',  file='chi', tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='U',    file='u',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='V',    file='v',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='OMGF', file='w',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='T',    file='t',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='Z',    file='z',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='PS',   file='p',   tintv=1, tavrg=1, tunit='DAY' &end
END_OF_DATA
#
#  run
#
$RUN < $DIR/SYSIN >> $DIR/SYSOUT
#
cd $TDIR
$TRANS
echo job end at `date` >> $DIR/SYSOUT
