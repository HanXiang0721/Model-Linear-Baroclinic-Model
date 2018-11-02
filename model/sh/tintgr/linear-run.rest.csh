#!/bin/csh -f
#
#      sample script for linear model run (moist model)
#      output is daily average
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
setenv RSTFILE  $DIR/Restart.amat                      # Restart-Data File
#
# 3D (win)
#setenv DATZ     $LNHOME/bs/gt3/grz.t21                 # Topography
#setenv DATS     $LNHOME/bs/gt3/sstwin                  # mean SST
#setenv DATW     $LNHOME/bs/gt3/wgwin                   # soil wetness
#setenv DATI     $LNHOME/bs/gt3/gridx.t21               # sfc. index
#setenv BSFILE $LNHOME/bs/gt3/ncepwin.t21l20            # Atm. BS File
#
# state of rest
setenv DATZ     
#setenv DATS     $LNHOME/bs/gt3/sst.rest                 # mean SST
setenv DATS     $LNHOME/bs/gt3/sst.rest.wn1             # mean SST
setenv DATW     
setenv DATI     
#setenv BSFILE $LNHOME/bs/gt3/rest.t21l20                # Atm. BS File
setenv BSFILE $LNHOME/bs/gt3/rest.t21l20.wn1            # Atm. BS File
#
# forcing
#
setenv FRC      $FDIR/frcsst.rest.t21.grd               # initial perturbation
#setenv SFRC     $FDIR/frcsst.rest.t21.grd               # steady forcing
#setenv SFRC     $FDIR/frcsst.restwn1.t21.grd            # steady forcing
#setenv SFRC     $FDIR/frcsst.rest.t21.grd               # steady forcing
#setenv SFRC     $FDIR/frcsst.rest.t21.w.grd             # steady forcing
setenv SFRC     $FDIR/frcsst.rest.t21.e.grd             # steady forcing
#
# duration of integration
#
setenv TEND     21
#
if (! -e $DIR) mkdir -p $DIR
cd $DIR
/bin/rm -f SYSOUT
/bin/rm -f psi chi u v w t z p q dtc dqc dtl dql pr
echo job started at `date` > $DIR/SYSOUT
/bin/rm -f $DIR/SYSIN
#
#      parameters
#
cat << END_OF_DATA >>! $DIR/SYSIN

 &nmrun  run='linear model'                                     &end
 &nmtime start=0,1,1,0,0,0, end=0,1,$TEND,0,0,0                 &end
 &nmdelt delt=40, tunit='MIN', inistp=2                         &end
 &nmhdif order=4, tefold=24, tunit='HOUR'                       &end
 &nmdamp ddragv=1.,1.,1.,5.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,1.,1.,
         ddragd=1.,1.,1.,5.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,1.,1.,
         ddragt=1.,1.,1.,5.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,1.,1.,
         ddragq=1.,1.,1.,5.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                            &end
 &nminit file='$BSFILE' , DTBFR=0., DTAFTR=0., TUNIT='DAY'      &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON',  overwt=t       &end
 &nmdata item='GRZ',    file='$DATZ'                            &end
 &nmdata item='GRSST',  file='$DATS'                            &end
 &nmdata item='GRIDX',  file='$DATI'                            &end
 &nmdata item='GRWG',   file='$DATW'                            &end

 &nmbtdif tdmpc=0.                                              &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3                                   &end
 &nmmca ocum=t, ttau0=3, qtau0=3, ttauc=6, tunit='HOUR', sigkb=0.98D0 &end 
 &nmlsc olsc=f, ttaul0=2, qtaul0=2, tunit='HOUR', dqrat=1.d-2 &end
 &nmsfcm owes=t, expw=0.5                                      &end

 &nmchck ocheck=f, ockall=f                                     &end

 &nmfrc  ffrc='$FRC',   oper=f, nfcs=1                          &end
 &nmsfrc fsfrc='$SFRC', ofrc=t, osstf=t, nsfcs=1, fsend=-1,1,4,0,0,0     &end

 &nmhisd tintv=1, tavrg=1, tunit='DAY'                          &end
 &nmhist item='PSI',  file='psi', tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='CHI',  file='chi', tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='U',    file='u',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='V',    file='v',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='OMGF', file='w',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='T',    file='t',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='Z',    file='z',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='PS',   file='p',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='Q',    file='q',   tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='Q1C',  file='dtc', tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='Q2C',  file='dqc', tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='Q1L',  file='dtl', tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='Q2L',  file='dql', tintv=1, tavrg=1, tunit='DAY' &end
 &nmhist item='PR',   file='pr',  tintv=1, tavrg=1, tunit='DAY' &end
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
