#!/bin/csh -f
#
#      sample script for linear model run (dry model)
#
# NQS command for mail
#@$-q short
#
setenv LNHOME   /home/hiro/ln_solver                   # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYSTEM   sun                                    # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t21ml20ctintgr # Excutable file
setenv FDIR     $LNHOME/data/frc                       # Directory for Output
setenv DIR      $LNHOME/data/out                       # Directory for Output
setenv BSFILE   $LNHOME/bs/gt3/rest.t21l20             # Atm. BS File
setenv RSTFILE  $DIR/Restart.amat                      # Restart-Data File
setenv FRC      $FDIR/frc.restq.grd                    # initial perturbation
##setenv SFRC     $FDIR/frc.restq.grd                    # steady forcing
setenv SFRC     $FDIR/frc.restqall.grd                    # steady forcing
setenv TEND     21
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

 &nmrun  run='linear model'                                 &end
 &nmtime start=0,1,1,0,0,0, end=0,1,$TEND,0,0,0             &end
 &nmdelt delt=40, tunit='MIN', inistp=2                     &end
 &nmhdif order=4, tefold=24, tunit='HOUR'                   &end
 &nmdamp ddragv=1.,1.,1.,5.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,1.,1.,
         ddragd=1.,1.,1.,5.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,1.,1.,
         ddragt=1.,1.,1.,5.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,1.,1.,
         tunit='DAY'                                           &end
 &nminit file='$BSFILE' , DTBFR=0., DTAFTR=0., TUNIT='DAY'   &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON',  overwt=t    &end

 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,                                   &end

 &nmfrc  ffrc='$FRC',   oper=f, nfcs=1                       &end
 &nmsfrc fsfrc='$SFRC', ofrc=t, nsfcs=1, fsend=-1,1,30,0,0,0 &end

 &nmchck ocheck=f, ockall=f                                  &end
 &nmdata item='GRZ',    file=''                              &end

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
cd $LNHOME/solver/util
./gt2gr
#
echo job end at `date` >> $DIR/SYSOUT
