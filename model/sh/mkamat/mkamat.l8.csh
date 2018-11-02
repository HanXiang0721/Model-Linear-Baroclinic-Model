#!/bin/csh -f
#
#      making linear matrix for T21L8 LBM/PWM
#
setenv LNHOME   /home/tropics/hiro/ln_solver           # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYSTEM   sun                                    # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t21ml8cmkamat # Excutable file
setenv DIR      $LNHOME/matrix                         # Directory for Output
#
# basic states, winter 3D
setenv DATZ     $LNHOME/bs/gt3/grz.t21                 # Topography
setenv DATS     $LNHOME/bs/gt3/sstwin                  # mean SST
setenv DATW     $LNHOME/bs/gt3/wgwin                   # soil wetness
setenv DATI     $LNHOME/bs/gt3/gridx.t21               # sfc. index
setenv BSFILE   $LNHOME/bs/gt3/ncepwin.t21l8           # Atm. BS File
#
setenv RSTFILE  $DIR/out/Restart.amat                  # Restart-Data File
setenv MATVDIR  $DIR/MAT_v                             # culumn matrices 
setenv MATDDIR  $DIR/MAT_d                             # culumn matrices 
setenv MATTDIR  $DIR/MAT_t                             # culumn matrices 
setenv MATPDIR  $DIR/MAT_p                             # culumn matrices 
setenv MATQDIR  $DIR/MAT_q                             # culumn matrices 
setenv REDIDIR  $LNHOME/solver/util
setenv REDIEXE  $REDIDIR/redist
setenv OVOR     TRUE
setenv ODIV     TRUE
setenv OTMP     TRUE
setenv OLNP     TRUE
setenv OSPH     TRUE
#
# T21L8m5
#
setenv NLEVS     8   # no. of vertical levels
setenv NWSTR     0   # first zonal wave number
setenv NWEND     5   # truncate wave number
setenv KORD      4
setenv KTEF      6
#
setenv RAYLE  100000
setenv RAYLC     5 # Rayleigh for critical latitudes
setenv STM       1
setenv STD       1
#
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
echo VOR zonal wave M is $izwv and LEVEL is $ilev
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $DIR/out) mkdir -p $DIR/out
if (! -e $MATVDIR$ilev) mkdir -p $MATVDIR$ilev
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
 &nmhdif order=$KORD, tefold=$KTEF, tunit='HOUR', oraycl=f, ray0=$RAYLE, raycl0=$RAYLC    &end
 &nmdamp ddragv=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragd=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragt=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragq=1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,
          zmfctq=1,1,1,1,1,1,1,1                               &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'  &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmmca ttau0=3, qtau0=3, ttauc=6, tunit='HOUR', sigkb=1.D0    &end
 &nmsfcm owes=t, expw=0.5                                      &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3         &end

 &nmamat amatf='$MATVDIR$ilev/AMATRIX$izwv', omatv=t, omatd=f, omatt=f, omatp=f, omatq=f, kmatw=$izwv, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'                           &end
 &nmdata item='GRSST',  file='$DATS'                           &end
 &nmdata item='GRIDX',  file='$DATI'                           &end
 &nmdata item='GRWG',   file='$DATW'                           &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
echo job end at `date` >> $DIR/out/SYSOUT$izwv
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
echo DIV zonal wave M is $izwv and LEVEL is $ilev
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $MATDDIR$ilev) mkdir -p $MATDDIR$ilev
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
 &nmhdif order=$KORD, tefold=$KTEF, tunit='HOUR', oraycl=f, ray0=$RAYLE, raycl0=$RAYLC    &end
 &nmdamp ddragv=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragd=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragt=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragq=1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,
          zmfctq=1,1,1,1,1,1,1,1                               &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'  &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmmca ttau0=3, qtau0=3, ttauc=6, tunit='HOUR', sigkb=1.D0    &end
 &nmsfcm owes=t, expw=0.5                                      &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3         &end

 &nmamat amatf='$MATDDIR$ilev/AMATRIX$izwv', omatv=f, omatd=t, omatt=f, omatp=f, omatq=f, kmatw=$izwv, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'                           &end
 &nmdata item='GRSST',  file='$DATS'                           &end
 &nmdata item='GRIDX',  file='$DATI'                           &end
 &nmdata item='GRWG',   file='$DATW'                           &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
echo job end at `date` >> $DIR/out/SYSOUT$izwv
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
echo TMP zonal wave M is $izwv and LEVEL is $ilev
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $MATTDIR$ilev) mkdir -p $MATTDIR$ilev
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
 &nmhdif order=$KORD, tefold=$KTEF, tunit='HOUR', oraycl=f, ray0=$RAYLE, raycl0=$RAYLC    &end
 &nmdamp ddragv=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragd=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragt=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragq=1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,
          zmfctq=1,1,1,1,1,1,1,1                               &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'  &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmmca ttau0=3, qtau0=3, ttauc=6, tunit='HOUR', sigkb=1.D0    &end
 &nmsfcm owes=t, expw=0.5                                      &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3         &end

 &nmamat amatf='$MATTDIR$ilev/AMATRIX$izwv', omatv=f, omatd=f, omatt=t, omatp=f, omatq=f, kmatw=$izwv, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'                           &end
 &nmdata item='GRSST',  file='$DATS'                           &end
 &nmdata item='GRIDX',  file='$DATI'                           &end
 &nmdata item='GRWG',   file='$DATW'                           &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
echo job end at `date` >> $DIR/out/SYSOUT$izwv
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
echo LnPs zonal wave M is $izwv and LEVEL is $ilev
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $MATPDIR$ilev) mkdir -p $MATPDIR$ilev
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
 &nmhdif order=$KORD, tefold=$KTEF, tunit='HOUR', oraycl=f, ray0=$RAYLE, raycl0=$RAYLC    &end
 &nmdamp ddragv=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragd=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragt=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragq=1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,
          zmfctq=1,1,1,1,1,1,1,1                               &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'  &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmmca ttau0=3, qtau0=3, ttauc=6, tunit='HOUR', sigkb=1.D0    &end
 &nmsfcm owes=t, expw=0.5                                      &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3         &end

 &nmamat amatf='$MATPDIR$ilev/AMATRIX$izwv', omatv=f, omatd=f, omatt=f, omatp=t, omatq=f, kmatw=$izwv, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'                           &end
 &nmdata item='GRSST',  file='$DATS'                           &end
 &nmdata item='GRIDX',  file='$DATI'                           &end
 &nmdata item='GRWG',   file='$DATW'                           &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
echo job end at `date` >> $DIR/out/SYSOUT$izwv
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
echo SPH zonal wave M is $izwv and LEVEL is $ilev
#
#      setup
#
if (! -e $DIR) mkdir -p $DIR
if (! -e $MATQDIR$ilev) mkdir -p $MATQDIR$ilev
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
 &nmhdif order=$KORD, tefold=$KTEF, tunit='HOUR', oraycl=f, ray0=$RAYLE, raycl0=$RAYLC    &end
 &nmdamp ddragv=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragd=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragt=1.,1.,10.,30.,30.,30.,30.,2.,
         ddragq=1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,
          zmfctq=1,1,1,1,1,1,1,1                               &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'  &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end
 &nmmca ttau0=3, qtau0=3, ttauc=6, tunit='HOUR', sigkb=1.D0    &end
 &nmsfcm owes=t, expw=0.5                                      &end
 &nmchck ocheck=t, ockall=f                                    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3         &end

 &nmamat amatf='$MATQDIR$ilev/AMATRIX$izwv', omatv=f, omatd=f, omatt=f, omatp=f, omatq=t, kmatw=$izwv, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'                           &end
 &nmdata item='GRSST',  file='$DATS'                           &end
 &nmdata item='GRIDX',  file='$DATI'                           &end
 &nmdata item='GRWG',   file='$DATW'                           &end

END_OF_DATA
#
#  run
#
( $RUN < $DIR/out/SYSIN >> $DIR/out/SYSOUT$izwv ) >& $DIR/out/ERROUT
echo job end at `date` >> $DIR/out/SYSOUT$izwv
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
cd $REDIDIR
$REDIEXE
echo redist end at `date` 
