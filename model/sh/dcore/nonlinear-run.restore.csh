#!/bin/csh -f
#
#      sample script for nonlinear model run (dry model)
#
# NQS command for mail
#@$-q short
#
setenv LNHOME   /home/hiro/ln_solver                   # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYSTEM   sun                                    # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t21ml20cdcore # Excutable file
setenv FDIR     $LNHOME/data/frc                       # Directory for Output
setenv DIR      $LNHOME/data/frc                       # Directory for Output
setenv INITFILE $LNHOME/bs/gt3/ncepsum.t21l20zm        # Atm. BS File
setenv RSTFILE  $DIR/Restart.amat                      # Restart-Data File
setenv DATZ     $LNHOME/bs/gt3/grz.t21                 # topography
setenv RFRC                                            # residual forcing
setenv SFRC     $LNHOME/bs/grads/ncepsum.t21l20.grd    # restoring state
setenv TEND     31
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
# &nmret drstt=
#  5.,5.1,5.3,6.2,9.7,
#  19.,27.,29.,30.,30.,
#  30.,30.,30.,30.,30.,
#  30.,30.,30.,30.,30., tunit='DAY'
# &end
#
cat << END_OF_DATA >>! $DIR/SYSIN

 &nmrun  run='nonlinear model'                              &end
 &nmtime start=0,1,1,0,0,0, end=0,1,$TEND,0,0,0             &end
 &nmdelt delt=40, tunit='MIN', inistp=2                     &end
 &nmhdif order=4, tefold=24, tunit='HOUR'                    &end
 &nminit file='$INITFILE' , DTBFR=0., DTAFTR=0., TUNIT='DAY' &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON',  overwt=t   &end

 &nmanm  oanm=f                                              &end
 &nmrfrc rsfrc='$RFRC'                                       &end
 &nmsfrc fsfrc='$SFRC'                                       &end
 &nmret drstt=
  2.,2.,2.5,3.,4.5,
  6.,8.,10.,15.,15.,
  15.,15.,15.,15.,15.,
  15.,15.,15.,15.,15., tunit='DAY'
 &end

 &nmchck ocheck=f, ockall=f                                  &end
 &nmdata item='GRZ',    file='$DATZ'                         &end

 &nmhisd tintv=1, tavrg=1, tunit='DAY'                       &end
 &nmhist item='PSI',  file='psi', tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='CHI',  file='chi', tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='U',    file='u',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='V',    file='v',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='OMGF', file='w',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='T',    file='t',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='Z',    file='z',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='PS',   file='p',   tintv=1, tavrg=1, tunit='DAY' &end

 &nmhist item='SLP',   file='slp',  tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='Q',     file='q',    tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='TFLUX', file='tflx', tintv=1, tavrg=1, tunit='DAY' &end
END_OF_DATA
#
#  run
#
( $RUN < $DIR/SYSIN >> $DIR/SYSOUT ) >& $DIR/ERROUT
echo job end at `date` >> $DIR/SYSOUT
