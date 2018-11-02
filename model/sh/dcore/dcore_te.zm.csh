#!/bin/csh -f
#
#      sample script for nonlinear model run (dry model)
#
#@$-q   r
#@$-N   1
#@$-me
#
setenv LNHOME   /home/q16655/ln_solver                 # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYS      sr8000                                 # execute system
setenv RUN      $LBMDIR/bin/$SYS/lbm2.t42ml20cdcore.te # Excutable file
setenv DIR      $LNHOME/data/dcore/gt3                 # Dir for Output
setenv FDIR     $LNHOME/data/dcore/te                  # Dir for Output
setenv INITFILE $LNHOME/bs/gt3/dcore_te.t42l20zm       # Atm. BS File
setenv RSTFILE  $DIR/Restart.00010901                  # Restart-Data File
setenv SFRC     $FDIR/te.t42l20zm.grd                  # rad-eq T
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
 &nmreq 
  drstq=1.,2.,5.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
  -1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
  drstrh=0.85,0.75,0.6,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
  -1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1., tuntq='DAY'
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
 &nmhist item='QFLUX',file='qflx',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='BI',   file='bi',  tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='DUVDF',file='duvdf',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='DVVDF',file='dvvdf',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='TAUX', file='taux',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='TAUY', file='tauy',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='Q',    file='q',   tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='QL',   file='ql',  tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='PRCP', file='prcp', tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='PRCPC',file='prcpc',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='PRCPL',file='prcpl',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='DTCUM',file='dtcum',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='DTLSC',file='dtlsc',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='DQCUM',file='dqcum',tintv=12, tavrg=12, tunit='HOUR' &end
 &nmhist item='DQLSC',file='dqlsc',tintv=12, tavrg=12, tunit='HOUR' &end
END_OF_DATA
#
#  run
#
( $RUN < $DIR/SYSIN >> $DIR/SYSOUT ) >& $DIR/ERROUT
echo job end at `date` >> $DIR/SYSOUT
