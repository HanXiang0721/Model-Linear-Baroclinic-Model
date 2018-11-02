#!/bin/csh -f
#
#      making linear matrix
#
# NQS command for mail
#@$-q   r
#@$-N   1
#@$-me
#
setenv LNHOME   /home/q16655/ln_solver                 # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv DIR      $LNHOME/data                           # Directory for Output
setenv MDIR     $LNHOME/matrix                         # Directory for Output
setenv REDIDIR  $LNHOME/solver/util
setenv CUSTDIR  $LNHOME/solver/custom
#
# month in basic states
#
setenv IMM      1
#
# horizontal resolution
#
setenv HRES     21
#
setenv SYSTEM   sr8000                                 # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t$HRES\ml11cmkamat # Excutable file
setenv REDIEXE  $REDIDIR/redist
setenv CUSTEXE  $CUSTDIR/inv
setenv RSTFILE  $DIR/out/Restart.amat                   # Restart-Data File
setenv DATZ     $LNHOME/data/couple/bs/gt3/grz.t$HRES   # Topography
setenv DATI     $LNHOME/data/couple/bs/gt3/gridx.t$HRES # sfc. index
#
# character month automatically defined
#
if( $IMM == 1  ) set CMON = jan ; if( $IMM == 2  ) set CMON = feb 
if( $IMM == 3  ) set CMON = mar ; if( $IMM == 4  ) set CMON = apr 
if( $IMM == 5  ) set CMON = may ; if( $IMM == 6  ) set CMON = jun
if( $IMM == 7  ) set CMON = jul ; if( $IMM == 8  ) set CMON = aug
if( $IMM == 9  ) set CMON = sep ; if( $IMM == 10 ) set CMON = oct
if( $IMM == 11 ) set CMON = nov ; if( $IMM == 12 ) set CMON = dec
#
# set basic states
#
setenv DATS    $LNHOME/data/couple/bs.t$HRES/gt3/sst$CMON.t$HRES # mean SST
setenv DATW    $LNHOME/data/couple/bs.t$HRES/gt3/wg$CMON.t$HRES  # soil wetness
setenv BSFILE  $LNHOME/data/couple/bs.t$HRES/gt3/ncep$CMON.t$HRES\l11 # Atm. BS File
#
# set matrix file
#
setenv MATFIL   MAT.t$HRES\l11m15.ncp$CMON.dat              # matrix L
setenv MATIFIL  MATINV.t$HRES\l11m15.ncp$CMON.dat           # matrix L-1
#
# log
#
setenv LOG      $LBMDIR/sh/LOG
#
setenv MATVDIR  $MDIR/MAT_v                             # culumn matrices 
setenv MATDDIR  $MDIR/MAT_d                             # culumn matrices 
setenv MATTDIR  $MDIR/MAT_t                             # culumn matrices 
setenv MATPDIR  $MDIR/MAT_p                             # culumn matrices 
setenv MATQDIR  $MDIR/MAT_q                             # culumn matrices 
setenv OVOR     TRUE
setenv ODIV     TRUE
setenv OTMP     TRUE
setenv OLNP     TRUE
setenv OSPH     TRUE
#
# T21 L11
#
setenv NLEVS    11   # no. of vertical levels
setenv NWSTR     0   # first zonal wave number
setenv NWEND    15   # truncate wave number
#setenv NWEND     6   # truncate wave number
setenv KORD      4
setenv KTEF      6
#
setenv RAYLE  100000
setenv RAYLC     5 # Rayleigh for critical latitudes
setenv STM       1
setenv STD       1
#
rm -f $LOG
echo job started at `date` > $LOG
/bin/rm -f $DIR/out/SYSOUT* $DIR/out/ERROUT
echo --- exec error messages --- > $DIR/out/ERROUT
#
#       resolution	
#
@ maxdl = 1
@ maxdw = 1
#
@ maxw = $NWEND
@ maxl = $NLEVS
#
@ maxpl = 1
#
#	loop for a unity (NMDIM x KMAX) of vor.
#
if( $OVOR == TRUE ) then
@ ilev = 1
@ izwv = $NWSTR
@ izwv = $izwv - 1
while ( $ilev <= $maxl )
   @ izwv = $izwv + 1
#
echo VOR zonal wave M is $izwv and LEVEL is $ilev >> $LOG
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $DIR/out) mkdir -p $DIR/out
if (! -e $MATVDIR$ilev) mkdir -p $MATVDIR$ilev
cd $DIR
/bin/rm -f $DIR/out/SYSIN
#
#      parameters
#
# changed: drag 0.5,5->0.25,5, tauc 6->8
# changed: dlnd=2 -> dlnd=4, drag 30,30,2->30,1,1
# upper-level drag was critical for reasonance near EA
# changed: dlnd=4 -> dlnd=2, tauc 8->6, drag=0.25,0.25,0.25->3*0.5
# changed: drag all 1,1,1,30,30,30,....,1
#
# changed: tauc 6->4.5, cefmx .9->.8
#
# changed: tauc 4.5->5, drag 1,1,1,30,.. -> .5,.5,.5,2.,5.,30.,..
#
# changed: drag .5,.5,.5,2.,5.,30.,. -> .5,.5,.5,5.,20.,50.,..
#
# changed: cefmx 0.8 -> 0.9
#
# &nmdamp ddragv=0.25,0.25,0.5,2.,5.,30.,30.,30.,30.,30.,2.,
# &nmdamp ddragv=0.5,0.5,0.5,5.,10.,30.,30.,30.,30.,1.,1.,
#         ddragd=0.5,0.5,0.5,5.,10.,30.,30.,30.,30.,1.,1.,
#         ddragt=1.,1.,1.,5.,10.,30.,30.,30.,30.,1.,1.,
#         ddragq=1.,1.,1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
# as of 04/05/31
# &nmdamp ddragv=0.5,0.5,0.5,5.,20.,50.,50.,50.,50.,1.,1.,
#         ddragd=0.5,0.5,0.5,5.,20.,50.,50.,50.,50.,1.,1.,
#         ddragt=0.5,0.5,0.5,5.,20.,50.,50.,50.,50.,1.,1.,
#         ddragq=0.5,0.5,0.5,5.,20.,50.,50.,50.,50.,1.,1.,
# changed as of 04/06/02
# ocef=t -> ocef=f, dlnd=2 -> dlnd=5
#
# dragq 1,2,3,15,-1,.. --> -1,-1,-1,-1,-1,... (due to bug fix in dintg-2.F)
#
# no drag to q after bug in dintg-2.F fixed on 06/20/04
#
# as of 2004/07/08 for improving basin avg U
# dlnd=5 -> 1 , zsth=200->1d8 (no stress damping)
# thcape=3.d3(default) -> 3.5d3
# drag(vdt):   -1.,-1.,-1.,-1.,-1.,-1.,-1.
#           -> 30.,30.,30.,30.,30.,30.,2.,
# as of 2004/07/12 for improving basin avg U
# dlnd=1 -> 2 
# drag(vdt):   30.,30.,30.,30.,30.,30.,2.,
#           -> -1.,-1.,-1.,-1.,-1.,-1.,2.
# drag(t, top):   2 -> -1
# drag(vd, top):  2 -> 10
#
cat << END_OF_DATA >>! $DIR/out/SYSIN

 &nmrun  run='make matrix'                                     &end
 &nmtime start=0,$STM,$STD,0,0,0, end=0,$STM,$STD,0,30,0       &end
 &nmdelt delt=40, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='HOUR', oraycl=f, ray0=$RAYLE, raycl0=$RAYLC    &end
 &nmdamp ddragv=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,10.,
         ddragd=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,10.,
         ddragt=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         ddragq=-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,1,1,1,
          zmfctq=1,1,1,1,1,1,1,1,1,1,1                         &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'    &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmmca ttau0=3, qtau0=3, ttauc=5., tunit='HOUR', 
        sigkb=0.95D0, thcape=3.5d3, ocef=t, cefmx=0.9          &end
 &nmsfcm expw=0.5, dlnd=2., owes=t, zsth=1.d8                  &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3  &end

 &nmamat amatf='$MATVDIR$ilev/AMATRIX$izwv', omatv=t, omatd=f, omatt=f, omatp=f, omatq=f, kmatw=$izwv, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'                            &end
 &nmdata item='GRSST',  file='$DATS'                            &end
 &nmdata item='GRIDX',  file='$DATI'                            &end
 &nmdata item='GRWG',   file='$DATW'                            &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
#
   if ( $maxw <= $izwv) then
      @ izwv = $NWSTR
      @ izwv = $izwv - 1
      @ ilev++
   endif
end
endif
#
#	loop for a unity (NMDIM x KMAX) of div.
#
if( $ODIV == TRUE ) then
@ ilev = 1
@ izwv = $NWSTR
@ izwv = $izwv - 1
while ( $ilev <= $maxl )
   @ izwv = $izwv + 1
#
echo DIV zonal wave M is $izwv and LEVEL is $ilev >> $LOG
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $MATDDIR$ilev) mkdir -p $MATDDIR$ilev
cd $DIR
/bin/rm -f $DIR/out/SYSIN
#
#      parameters
#
cat << END_OF_DATA >>! $DIR/out/SYSIN

 &nmrun  run='make matrix'                                     &end
 &nmtime start=0,$STM,$STD,0,0,0, end=0,$STM,$STD,0,30,0       &end
 &nmdelt delt=40, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='HOUR', oraycl=f, ray0=$RAYLE, raycl0=$RAYLC    &end
 &nmdamp ddragv=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,10.,
         ddragd=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,10.,
         ddragt=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         ddragq=-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,1,1,1,
          zmfctq=1,1,1,1,1,1,1,1,1,1,1                         &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'    &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmmca ttau0=3, qtau0=3, ttauc=5., tunit='HOUR', 
        sigkb=0.95D0, thcape=3.5d3, ocef=t, cefmx=0.9          &end
 &nmsfcm expw=0.5, dlnd=2., owes=t, zsth=1.d8                  &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3  &end

 &nmamat amatf='$MATDDIR$ilev/AMATRIX$izwv', omatv=f, omatd=t, omatt=f, omatp=f, omatq=f, kmatw=$izwv, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'                            &end
 &nmdata item='GRSST',  file='$DATS'                            &end
 &nmdata item='GRIDX',  file='$DATI'                            &end
 &nmdata item='GRWG',   file='$DATW'                            &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
#
   if ( $maxw <= $izwv) then
      @ izwv = $NWSTR
      @ izwv = $izwv - 1
      @ ilev++
   endif
end
endif
#
#	loop for a unity (NMDIM x KMAX) of temp.
#
if( $OTMP == TRUE ) then
@ ilev = 1
@ izwv = $NWSTR
@ izwv = $izwv - 1
while ( $ilev <= $maxl )
   @ izwv = $izwv + 1
#
echo TMP zonal wave M is $izwv and LEVEL is $ilev >> $LOG
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $MATTDIR$ilev) mkdir -p $MATTDIR$ilev
cd $DIR
/bin/rm -f $DIR/out/SYSIN
#
#      parameters
#
cat << END_OF_DATA >>! $DIR/out/SYSIN

 &nmrun  run='make matrix'                                     &end
 &nmtime start=0,$STM,$STD,0,0,0, end=0,$STM,$STD,0,30,0       &end
 &nmdelt delt=40, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='HOUR', oraycl=f, ray0=$RAYLE, raycl0=$RAYLC    &end
 &nmdamp ddragv=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,10.,
         ddragd=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,10.,
         ddragt=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         ddragq=-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,1,1,1,
          zmfctq=1,1,1,1,1,1,1,1,1,1,1                         &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'    &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmmca ttau0=3, qtau0=3, ttauc=5., tunit='HOUR', 
        sigkb=0.95D0, thcape=3.5d3, ocef=t, cefmx=0.9          &end 
 &nmsfcm expw=0.5, dlnd=2., owes=t, zsth=1.d8                  &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3  &end

 &nmamat amatf='$MATTDIR$ilev/AMATRIX$izwv', omatv=f, omatd=f, omatt=t, omatp=f, omatq=f, kmatw=$izwv, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'                            &end
 &nmdata item='GRSST',  file='$DATS'                            &end
 &nmdata item='GRIDX',  file='$DATI'                            &end
 &nmdata item='GRWG',   file='$DATW'                            &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
#
   if ( $maxw <= $izwv) then
      @ izwv = $NWSTR
      @ izwv = $izwv - 1
      @ ilev++
   endif
end
endif
#
#	loop for a unity (NMDIM x KMAX) of LnPs
#
if( $OLNP == TRUE ) then
@ ilev = 1
@ izwv = $NWSTR
@ izwv = $izwv - 1
while ( $ilev <= $maxpl )
   @ izwv = $izwv + 1
#
echo LnPs zonal wave M is $izwv and LEVEL is $ilev >> $LOG
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $MATPDIR$ilev) mkdir -p $MATPDIR$ilev
cd $DIR
/bin/rm -f $DIR/out/SYSIN
#
#      parameters
#
cat << END_OF_DATA >>! $DIR/out/SYSIN

 &nmrun  run='make matrix'                                     &end
 &nmtime start=0,$STM,$STD,0,0,0, end=0,$STM,$STD,0,30,0       &end
 &nmdelt delt=40, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='HOUR', oraycl=f, ray0=$RAYLE, raycl0=$RAYLC    &end
 &nmdamp ddragv=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,10.,
         ddragd=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,10.,
         ddragt=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         ddragq=-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,1,1,1,
          zmfctq=1,1,1,1,1,1,1,1,1,1,1                         &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'    &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmmca ttau0=3, qtau0=3, ttauc=5., tunit='HOUR', 
        sigkb=0.95D0, thcape=3.5d3, ocef=t, cefmx=0.9          &end
 &nmsfcm expw=0.5, dlnd=2., owes=t, zsth=1.d8                  &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3  &end

 &nmamat amatf='$MATPDIR$ilev/AMATRIX$izwv', omatv=f, omatd=f, omatt=f, omatp=t, omatq=f, kmatw=$izwv, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'                            &end
 &nmdata item='GRSST',  file='$DATS'                            &end
 &nmdata item='GRIDX',  file='$DATI'                            &end
 &nmdata item='GRWG',   file='$DATW'                            &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
#
   if ( $maxw <= $izwv) then
      @ izwv = $NWSTR
      @ izwv = $izwv - 1
      @ ilev++
   endif
end
endif
#
#	loop for a unity (NMDIM x KMAX) of humidity
#
if( $OSPH == TRUE ) then
@ ilev = 1
@ izwv = $NWSTR
@ izwv = $izwv - 1
while ( $ilev <= $maxl )
   @ izwv = $izwv + 1
#
echo SPH zonal wave M is $izwv and LEVEL is $ilev >> $LOG
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $MATQDIR$ilev) mkdir -p $MATQDIR$ilev
cd $DIR
/bin/rm -f $DIR/out/SYSIN
#
#      parameters
#
cat << END_OF_DATA >>! $DIR/out/SYSIN

 &nmrun  run='make matrix'                                     &end
 &nmtime start=0,$STM,$STD,0,0,0, end=0,$STM,$STD,0,30,0       &end
 &nmdelt delt=40, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='HOUR', oraycl=f, ray0=$RAYLE, raycl0=$RAYLC    &end
 &nmdamp ddragv=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,10.,
         ddragd=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,10.,
         ddragt=1.,2.,3.,15.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         ddragq=-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,1,1,1,
          zmfctq=1,1,1,1,1,1,1,1,1,1,1                         &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'    &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmmca ttau0=3, qtau0=3, ttauc=5., tunit='HOUR',
        sigkb=0.95D0, thcape=3.5d3, ocef=t, cefmx=0.9          &end
 &nmsfcm expw=0.5, dlnd=2., owes=t, zsth=1.d8                  &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3  &end

 &nmamat amatf='$MATQDIR$ilev/AMATRIX$izwv', omatv=f, omatd=f, omatt=f, omatp=f, omatq=t, kmatw=$izwv, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'                            &end
 &nmdata item='GRSST',  file='$DATS'                            &end
 &nmdata item='GRIDX',  file='$DATI'                            &end
 &nmdata item='GRWG',   file='$DATW'                            &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
#
   if ( $maxw <= $izwv) then
      @ izwv = $NWSTR
      @ izwv = $izwv - 1
      @ ilev++
   endif
end
endif
#
#  redistribution
#
echo >> $LOG
echo redist start at `date`  >> $LOG
cd $REDIDIR
mv -f SETPAR SETPAR.org
cat << END_OF_DATA2 >>! SETPAR
 &nmred cdr='/home/q16655/ln_solver/matrix',
        cfo='/home/q16655/ln_solver/matrix.moi/mat/$MATFIL'
 &end
 &nmall owall=f
 &end
 &nmcls oclassic=f
 &end
END_OF_DATA2
$REDIEXE  >> LOG
mv -f SETPAR.org SETPAR
echo redist end at `date`  >> $LOG
#
#  matrix inversion
#
echo inverse L started at `date` >> $LOG
cd $CUSTDIR
mv -f SETPAR SETPAR.org
cat << END_OF_DATA3 >>! SETPAR
 &nmfin cfm='/home/q16655/ln_solver/matrix.moi/mat/$MATFIL',
        cfr='/home/q16655/ln_solver/data/couple/frc/frc.t21l11.mat',
        cfs='/home/q16655/ln_solver/matrix.moi/mat/$MATIFIL',
        cfg='/home/q16655/ln_solver/data/couple/rsp/rsp.l11m15.grd'
 &end
 &nmall owall=f
 &end
 &nmcls oclassic=f
 &end
END_OF_DATA3
#
#  run
#
$CUSTEXE  >> $LOG
mv -f SETPAR.org SETPAR
echo inverse L end at `date`  >> $LOG
#
echo job end at `date`  >> $LOG
#
exit 0
