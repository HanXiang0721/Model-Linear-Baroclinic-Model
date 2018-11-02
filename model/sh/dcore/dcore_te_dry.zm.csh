#!/bin/csh -f
#
#      sample script for nonlinear model run (dry model)
#
#@$-q   r
#@$-N   1
#@$-me
#
setenv DT       110
#setenv DT       90
#setenv DT       70
#setenv DT       50
#setenv DT       30
#setenv DT       20
#setenv DT       10
#
setenv LNHOME   /home/q16655/ln_solver                 # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYS      sr8000                                 # execute system
setenv RUN      $LBMDIR/bin/$SYS/lbm2.t42ml20cdcore.te # Excutable file
setenv DIR      $LNHOME/data/dcore/gt3                 # Dir for Output
setenv FDIR     $LNHOME/data/dcore/te                  # Dir for Output
setenv INITFILE $LNHOME/bs/gt3/dcore_te_T$DT.t42l20zm  # Atm. BS File
setenv RSTFILE  $DIR/Restart.00010901                  # Restart-Data File
setenv SFRC     $FDIR/te_T$DT.t42l20zm.grd             # rad-eq T
setenv TEND     601
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
 &nmdelt delt=20, tunit='MIN', inistp=2                     &end
 &nmhdif order=8, tefold=6, tunit='HOUR'                    &end
 &nminit file='$INITFILE' , DTBFR=0., DTAFTR=0., TUNIT='DAY' &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON',  overwt=t   &end

 &nmanm  oanm=f                                              &end
 &nmrfrc rsfrc='  '                                          &end
 &nmsfrc fsfrc='$SFRC'                                       &end

 &nmret drstt=
  5.,5.1,5.3,6.2,9.7,
  19.,27.,29.,30.,30.,
  30.,30.,30.,30.,30.,
  30.,30.,30.,30.,30., tunit='DAY'
 &end

 &nmchck ocheck=f, ockall=f                                  &end
 &nmdata item='GRZ',    file='     '                         &end

 &nmhisd tintv=1, tavrg=1, tunit='DAY'                       &end
 &nmhist item='PSI',  file='psi', tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='CHI',  file='chi', tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='VOR',  file='vor', tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='DIV',  file='div', tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='U',    file='u',   tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='V',    file='v',   tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='OMGF', file='w',   tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='T',    file='t',   tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='THETA',file='th',  tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='Z',    file='z',   tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='PS',   file='p',   tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='TFLUX',file='tflx',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='BI',   file='bi',  tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='DUVDF',file='duvdf',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='DVVDF',file='dvvdf',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='TAUX', file='taux',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='TAUY', file='tauy',tintv=12, tavrg=12, tunit='HOUR' &end
END_OF_DATA
#
#  run
#
( $RUN < $DIR/SYSIN >> $DIR/SYSOUT ) >& $DIR/ERROUT
echo job end at `date` >> $DIR/SYSOUT
