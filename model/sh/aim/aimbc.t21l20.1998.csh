#!/bin/csh -f
#
# test for AIM for baroclinic model
#      06/10/05
#      Hiro Watanabe
#
#      making linear matrix for T21L20 PWM 
#      then solve steady response
#
# Usage: before running this script complie as
#      %ln_solver/model/src/> make lbm
#      %ln_solver/solver/util> make
#      %ln_solver/solver/custom> make
#
# NQS command for mail
#@$-q   q
#@$-N   1
#@$-me
#
setenv LNHOME   /home/q16655/ln_solver                 # ROOT of model
setenv LBMDIR   $LNHOME/model                          # ROOT of LBM 
setenv SYSTEM   sr8000                                 # execute system
setenv DIR      $LNHOME/matrix                         # Directory for Outputs
setenv FRDIR    $LNHOME/data/aim/rspbc                 # Directory for Outputs
setenv MATVDIR  $DIR/MAT_v                             # culumn matrices 
setenv MATDDIR  $DIR/MAT_d                             # culumn matrices 
setenv MATTDIR  $DIR/MAT_t                             # culumn matrices 
setenv MATPDIR  $DIR/MAT_p                             # culumn matrices 
setenv REDIDIR  $LNHOME/solver/util
setenv INVDIR   $LNHOME/solver/custom
#
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t21ml20caim   # Excutable file
setenv REDIEXE  $REDIDIR/redist
setenv INVEXE   $INVDIR/inv
#
setenv MESSG    $LBMDIR/sh/message.aim
#
setenv BSFILE   $LNHOME/bs/gt3/erawin.t21l20           # Atm. BS File (ZM)
setenv DATZ     $LNHOME/bs/gt3/grz.t21                 # Topography
setenv MATFILE  $DIR/mat/MATPWM.t21l20.zm-r.dat        # Linear matrix
setenv MATIFILE $DIR/mat/MATINV.t21l20.zm-r.dat        # Linear inv. matrix
#
setenv FRCFILE  $FRDIR/frc.q1.anm.t21l20.grd           # forcing
setenv RSPFILT  $FRDIR/aim.t21l20.1998.q.grd           # response
#
setenv FIRSTYR  1960
setenv LASTYR   2002
setenv CHOSNYR  1998
#
#setenv OMKMATL  TRUE
#setenv OMKMATR  TRUE
#setenv OMKINV   TRUE
setenv OAIM     TRUE
#
setenv OMKMATL  FALSE
setenv OMKMATR  FALSE
setenv OMKINV   FALSE
#setenv OAIM     FALSE
#
# T21 L20
#
setenv NVAR      4   # no. of variables
setenv NLEVS    20   # no. of vertical levels
setenv NWSTR     0   # first zonal wave number
setenv NWEND    21   # truncate wave number
setenv KORD      4   # order of diffusion
setenv KTEF    0.5   # e-folding time in day
#
# gamnma (=parameter for R, define gamma > 0 unlike baro.aim) 
#
#setenv RFCT         0.0
setenv RFCT      4000.0 # 
setenv MAXITE     10000
#setenv ERRMN      1.d-3
#setenv ERRMN      5.d-3
setenv ERRMN      1.d-2
#setenv ERRMN      1.d-8
#
setenv STM       1
setenv STD       1
#
rm -f $MESSG 
rm -f $DIR/out/SYSOUT* $DIR/out/ERROUT 
echo Start AIM at `date` > $MESSG
echo --- error messages --- > $DIR/out/ERROUT
if( $OMKMATL == TRUE ) then
echo Start AIM matrix Ls part at `date` >> $MESSG
#
#       resolution	
#
@ maxv = $NVAR
@ maxw = $NWEND
#
#	loop for a unity (NMDIM x KMAX) of vor./div./tmp./pi
#
@ ivar = 1
while ( $ivar <= $maxv )
  @ maxl = $NLEVS
  echo ================================================= >> $MESSG
  echo ============== Making Matrix Ls ================= >> $MESSG
  echo ================================================= >> $MESSG
  if( $ivar == 1 ) then
     set CVAR = VOR
     set OMV = t
     set OMD = f
     set OMT = f
     set OMP = f
     set MATDIR = $MATVDIR
  endif
  if( $ivar == 2 ) then
     set CVAR = DIV
     set OMV = f
     set OMD = t
     set OMT = f
     set OMP = f
     set MATDIR = $MATDDIR
  endif
  if( $ivar == 3 ) then
     set CVAR = TMP
     set OMV = f
     set OMD = f
     set OMT = t
     set OMP = f
     set MATDIR = $MATTDIR
  endif
  if( $ivar == 4 ) then
     set CVAR = LnPs
     set OMV = f
     set OMD = f
     set OMT = f
     set OMP = t
     set MATDIR = $MATPDIR
     @ maxl = 1
  endif

  @ ilev = 1
  @ izwv = $NWSTR
  @ izwv = $izwv - 1
  while ( $ilev <= $maxl )
     @ izwv = $izwv + 1
#
     echo $CVAR zonal wave M is $izwv and LEVEL is $ilev >> $MESSG
#
#      setup
#
     if (! -e $DIR) mkdir -p $DIR
     if (! -e $DIR/out) mkdir -p $DIR/out
     if (! -e $MATDIR$ilev) mkdir -p $MATDIR$ilev
     cd $DIR
     echo job started at `date` > $DIR/out/SYSOUT$izwv
     /bin/rm -f $DIR/out/SYSIN
#
#      parameters
#
cat << END_OF_DATA >>! $DIR/out/SYSIN
 &nmrun  run='make matrix'                                     &end
 &nmuse  omkmat=t                                              &end
 &nmtime start=1,$STM,$STD,0,0,0, end=1,$STM,$STD,0,5,0        &end
 &nmdelt delt=10, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='DAY'                &end
 &nmmtrn otrun=t, mtrn=0                                       &end
 &nmntrm nstp=1                                                &end
 &nmdamp ddragv=0.5,0.5,0.5,1,5,20,20,20,20,20,20,20,20,20,20,20,20,20,0.5,0.5,
         ddragd=0.5,0.5,0.5,1,5,20,20,20,20,20,20,20,20,20,20,20,20,20,0.5,0.5,
         ddragt=0.5,0.5,0.5,1,5,20,20,20,20,20,20,20,20,20,20,20,20,20,0.5,0.5,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1       &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmsole oslnoid=t                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3                            &end
 &nmamat amatf='$MATDIR$ilev/WMATRIX$izwv', 
         omatv=$OMV, omatd=$OMD, omatt=$OMT, omatp=$OMP, 
         kmatw=$izwv, kmatl=$ilev 
 &end
 &nmdata item='GRZ',    file='$DATZ'                           &end
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
  @ ivar++
end
#
echo End AIM matrix Ls part at `date` > $MESSG
endif
#
#       add R to Ls
#
if( $OMKMATR == TRUE ) then
echo Start AIM matrix R part at `date` > $MESSG
@ maxv = $NVAR
@ maxw = $NWEND
#
#	loop for a unity (NMDIM x KMAX) of vor./div./tmp./pi
#
@ ivar = 1
while ( $ivar <= $maxv )
  @ maxl = $NLEVS
  echo ================================================= >> $MESSG
  echo ============== Adding R to Ls =================== >> $MESSG
  echo ================================================= >> $MESSG
  if( $ivar == 1 ) then
     set CVAR = VOR
     set OMV = t
     set OMD = f
     set OMT = f
     set OMP = f
     set MATDIR = $MATVDIR
  endif
  if( $ivar == 2 ) then
     set CVAR = DIV
     set OMV = f
     set OMD = t
     set OMT = f
     set OMP = f
     set MATDIR = $MATDDIR
  endif
  if( $ivar == 3 ) then
     set CVAR = TMP
     set OMV = f
     set OMD = f
     set OMT = t
     set OMP = f
     set MATDIR = $MATTDIR
  endif
  if( $ivar == 4 ) then
     set CVAR = LnPs
     set OMV = f
     set OMD = f
     set OMT = f
     set OMP = t
     set MATDIR = $MATPDIR
     @ maxl = 1
  endif

  @ ilev = 1
  @ izwv = $NWSTR
  @ izwv = $izwv - 1
  while ( $ilev <= $maxl )
     @ izwv = $izwv + 1
#
     echo $CVAR zonal wave M is $izwv and LEVEL is $ilev >> $MESSG
#
#      setup
#
     if (! -e $DIR) mkdir -p $DIR
     if (! -e $DIR/out) mkdir -p $DIR/out
     if (! -e $MATDIR$ilev) mkdir -p $MATDIR$ilev
     cd $DIR
     echo job started at `date` > $DIR/out/SYSOUT$izwv
     rm -f $DIR/out/SYSIN
#
#      parameters
#
cat << END_OF_DATA1 >>! $DIR/out/SYSIN
 &nmrun  run='make matrix'                                     &end
 &nmuse  omkmat=t                                              &end
 &nmtime start=1,$STM,$STD,0,0,0, end=1,$STM,$STD,0,5,0        &end
 &nmdelt delt=10, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='DAY'                &end
 &nmrdif rfact=$RFCT                                           &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'    &end
 &nmamat amatf='$MATDIR$ilev/WMATRIX$izwv', 
         omatv=$OMV, omatd=$OMD, omatt=$OMT, omatp=$OMP, 
         kmatw=$izwv, kmatl=$ilev 
 &end
 &nmbmat bmatf='$MATDIR$ilev/AMATRIX$izwv'                     &end
END_OF_DATA1
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
  @ ivar++
end
echo End AIM matrix R part at `date` > $MESSG
endif
#
#  redistribution
#
if( $OMKINV == TRUE ) then
cd $REDIDIR
#
#      parameters
#
mv -f SETPAR SETPAR.org
cat << END_OF_DATA2 >>! SETPAR
 &nmred cdr='$DIR',
        cfo='$MATFILE'
 &end
 &nmall owall=f
 &end
 &nmcls oclassic=t
 &end
END_OF_DATA2
#
$REDIEXE >> $MESSG
mv -f SETPAR.org SETPAR
echo redist end at `date` >> $MESSG
#
#  inversion
#
cd $INVDIR
#
#      parameters
#
mv -f SETPAR SETPAR.org
cat << END_OF_DATA3 >>! SETPAR
 &nmfin cfm='$MATFILE',
        cfr='$FRCFILE',
        cfs='$MATIFILE',
        cfg='$RSPFILT'
 &end
 &nmall owall=f
 &end
 &nmcls oclassic=t
 &end
END_OF_DATA3
#
$INVEXE >> $MESSG
mv -f SETPAR.org SETPAR
echo inverse end at `date`  >> $MESSG
endif
#
#  AIM
#
if( $OAIM == TRUE ) then
echo Start AIM iteration part >> $MESSG 
#
#  AIM
#
echo Start AIM iteration part >> $MESSG 
@ year = $FIRSTYR
@ iyear = 0
while ( $year <= $LASTYR ) 
@ iyear = $iyear + 1
if( $year == $CHOSNYR ) @ jyear = $iyear
@ year++
end
#
echo AIM for $CHOSNYR forcing, nsfcs= $jyear >> $MESSG
cd $FRDIR
rm -f $FRDIR/SYSIN $FRDIR/SYSOUT.aim
rm -f $RSPFILT
echo AIM started at `date` > $FRDIR/SYSOUT.aim
#
#      parameters
#
cat << END_OF_DATA4 >>! $FRDIR/SYSIN
 &nmrun  run='AIM'                                             &end
 &nmuse  omkmat=f                                              &end
 &nmtime start=1,$STM,$STD,0,0,0, end=1,$STM,$STD,0,5,0        &end
 &nmdelt delt=10, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='DAY'                &end
 &nmrdif rfact=$RFCT                                           &end
 &nmaim nitemx=$MAXITE, eps=$ERRMN                             &end
 &nmmtrn otrun=t, mtrn=0                                       &end
 &nmdamp ddragv=0.5,0.5,0.5,1,5,20,20,20,20,20,20,20,20,20,20,20,20,20,0.5,0.5,
         ddragd=0.5,0.5,0.5,1,5,20,20,20,20,20,20,20,20,20,20,20,20,20,0.5,0.5,
         ddragt=0.5,0.5,0.5,1,5,20,20,20,20,20,20,20,20,20,20,20,20,20,0.5,0.5,
         tunit='DAY'                                           &end
 &nmzmfct zmfctv=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          zmfctd=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          zmfctt=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1       &end
 &nminit file='$BSFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'    &end
 &nmbtdif tdmpc=0.                                             &end
 &nmsole oslnoid=t                                             &end
 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3                            &end
 &nmamat amatf='$MATIFILE', 
         fsfrc='$FRCFILE',
         nffst=$jyear,
         nflst=$jyear,
         nwtrn=$NWEND
 &end
 &nmrsp  frsp='$RSPFILT', oinit=f, ofdump=f                    &end
 &nms2p  os2p=t                                                &end
 &nmplev kplev=17, 
         plev=1000.,950.,850.,700.,600.,500.,400.,300.,
               250.,200.,150.,100., 70., 50., 30., 20., 10.
 &end
 &nmerr  oerr=f, ovd=t, ftru='  ', ferr='  '                   &end
 &nmdata item='GRZ',    file='$DATZ'                           &end
END_OF_DATA4
#
#  run
#
( $RUN < $FRDIR/SYSIN >> $FRDIR/SYSOUT.aim ) >& $FRDIR/ERROUT
grep CONVERGE $FRDIR/SYSOUT.aim  >> $MESSG
echo AIM end at `date` >> $FRDIR/SYSOUT.aim
endif
#
echo AIM end at `date` >> $MESSG
#
exit 0
