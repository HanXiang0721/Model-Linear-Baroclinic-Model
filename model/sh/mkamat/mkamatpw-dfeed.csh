#!/bin/csh -f
#
#      linear model
#      calculate dynamical feedback matrix
#
# NQS command for mail
#@$-q short
#
setenv LNHOME   /home/a/hiro/agcmH/ln_solver           # ROOT of model
setenv AGCMDIR  $LNHOME/model                          # ROOT of AGCM 
setenv LNLIBD   $LNHOME/etc                            # ROOT of LN lib
setenv SYSTEM   alpha                                  # execute system
#
# Excutable files
#
setenv RUN1     $AGCMDIR/bin/$SYSTEM/agcm5.t21ml20c.mkmat  
setenv RUN2     $LNLIBD/dfeed_lib1
setenv RUN3     $LNHOME/custom/covpw
setenv RUN4     $LNLIBD/dfeed_lib2
setenv RUN5     $AGCMDIR/bin/$SYSTEM/agcm5.t21ml20c.wvfrc  
setenv RUN6     $LNLIBD/dfeed_lib3
setenv RUN7     $LNLIBD/dfeed_lib4
#
# data files
#
setenv DATZ                                            # Topography Data
setenv DIR      $AGCMDIR/data/out_amat.pw              # Directory for Output
setenv INITFILE $AGCMDIR/init/init_ncepwin_t21l20sw    # Atm. basic state
setenv INITZMFILE $AGCMDIR/init/init_ncepwin_t21l20zm  # Atm. basic state
setenv BSGFILE  $LNHOME/bs/init_ncepwin_t21l20zm.grd   # Atm. basic state
setenv RSTFILE  $DIR/out/Restart.amat                  # Restart-Data File
setenv MATFIL   $DIR/lin/frc.dfeed0.l20.mat            # in/out forcing matrix
setenv MATFILS  $DIR/lin/frc.dfeed.l20.mat             # in/out forcing matrix
setenv EPFIL    $DIR/lin/rsp.dfeed0.l20.grd            # in/out EP div.
setenv EPFILS   $DIR/lin/rsp.dfeed.l20.grd             # in/out EP div.
setenv MATFEED  $DIR/matrix/DFEED_t21l20_m0.dat        # output feedback matrix
setenv AMATFIL  $DIR/matrix/AMATPW_ncepwin_t21l20.dat  # input linear operator
setenv AINVFIL  $DIR/matrix/AINVPW_ncepwin_t21l20.dat  # input inverse L
setenv BMATFIL  $DIR/matrix/AMATPW_ncepwin_t21l20_m0df.dat  # output linear operator
setenv IIVAR    1
#
setenv FACTV    1.D-7
setenv FACTD    1.D-7
setenv FACTT    1.D-3
setenv FACTP    1.D-5
#
# T21 L20 
#
setenv NLEVS   20   # no. of vertical levels
setenv LWAVE   21   # no. of meridional waves for m=0
setenv KORD     4
setenv KTEF     1
#
setenv RAYLE   100000
setenv STM     1
setenv STD     1
#
rm -f $DIR/out/SYSOUT*
#
#       loop for variable
#
echo ----------------
echo START EXECUTIONS
echo ----------------
@ irec = 0
#
@ maxv = 4
@ ivar = 1
while ( $ivar <= $maxv )
set ov1 = 'f'
set od1 = 'f'
set ot1 = 'f'
set op1 = 'f'
if( $ivar == 1 ) set ov1 = 't'
if( $ivar == 2 ) set od1 = 't'
if( $ivar == 3 ) set ot1 = 't'
if( $ivar == 4 ) set op1 = 't'
#
@ maxl = $NLEVS
if( $ivar == 4 ) @ maxl = 1
#
#	loop for a unity (NMDIM x KMAX) of each variable
#
   @ ilev = 1
   while ( $ilev <= $maxl )
   if( $ivar >= $IIVAR ) then
   if( $ivar == 1 ) echo put delta VOR at LEVEL $ilev
   if( $ivar == 2 ) echo put delta DIV at LEVEL $ilev
   if( $ivar == 3 ) echo put delta TMP at LEVEL $ilev
   if( $ivar == 4 ) echo put delta LNP at LEVEL $ilev
#########################################
#      forcing due to delta ZM
#########################################
cd $DIR
echo job started at `date` > $DIR/out/SYSOUT
/bin/rm -f $DIR/out/SYSIN
#
cat << END_OF_DATA1 >>! $DIR/out/SYSIN

 &nmrun  run='make matrix'                                     &end
 &nmtime start=0,$STM,$STD,0,0,0, end=0,$STM,$STD,0,30,0       &end
 &nmdelt delt=40, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='DAY', oraycl=f, ray0=$RAYLE  &end
 &nminit file='$INITFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'  &end
 &nminitzm file='$INITZMFILE'                                  &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end

 &nmchck ocheck=t, ockall=f                             &end
 &nmbtdif tdmpc=0.0                                            &end

 &nmamat amatf='$MATFIL', omatv=$ov1, omatd=$od1, omatt=$ot1, omatp=$op1, kmatw=0, kmatl=$ilev &end

 &nmdata item='GRZ',    file='$DATZ'            &end

END_OF_DATA1
#
#  run
#
$RUN1 < $DIR/out/SYSIN >> $DIR/out/SYSOUT
echo job end at `date` >> $DIR/out/SYSOUT
   endif
#
#########################################
#      solve for each L
#########################################
#
      @ maxw = $LWAVE
      @ iwav = 1
      while ( $iwav <= $maxw )
      @ irec = $irec + 1
   if( $ivar >= $IIVAR ) then
      echo  ... meridional wave $iwav 
#
      if( $ivar == 1 ) set dfact = $FACTV
      if( $ivar == 2 ) set dfact = $FACTD
      if( $ivar == 3 ) set dfact = $FACTT
      if( $ivar == 4 ) set dfact = $FACTP
#
cd $LNLIBD
rm -f PARA LOG $MATFILS
cat << END_OF_DATA2 >>! $LNLIBD/PARA
 &nmfin cfi='$MATFIL',
        cfo='$MATFILS',
        mwav=0,
        lwav=$iwav
 &end
END_OF_DATA2
$RUN2 > LOG
#
#########################################
#      solve for waves
#########################################
#
cd $LNHOME/custom
rm -f LOG SETPAR
cat << END_OF_DATA3 >>! $LNHOME/custom/SETPAR

 &nmfin cfm='$AMATFIL',
        cfr='$MATFILS',
        cfs='$AINVFIL',
        cfg='$EPFIL'
 &end

 &nmbs  cbs='$BSGFILE'
 &end

 &nmuv  o2u=t, o2v=t, opl=f, ozinc=f
 &end

END_OF_DATA3
#
#  run
#
$RUN3 > LOG
#
cd $LNLIBD
rm -f PARA 
cat << END_OF_DATA4 >>! $LNLIBD/PARA
 &nmfin cfi='$EPFIL',
        cfb='$BSGFILE',
        cfo='$EPFILS'
 &end
END_OF_DATA4
$RUN4 >> LOG
#
#########################################
#      forcing due to EP divergence
#########################################
cd $DIR
echo job started at `date` > $DIR/out/SYSOUT
/bin/rm -f $DIR/out/SYSIN $MATFILS
#
cat << END_OF_DATA5 >>! $DIR/out/SYSIN

 &nmrun  run='make matrix'                                     &end
 &nmtime start=0,$STM,$STD,0,0,0, end=0,$STM,$STD,0,30,0       &end
 &nmdelt delt=40, tunit='MIN', inistp=0                        &end
 &nmhdif order=$KORD, tefold=$KTEF, tunit='DAY', oraycl=f, ray0=$RAYLE  &end
 &nminit file='$INITFILE' , DTBFR=1., DTAFTR=1., TUNIT='YEAR'  &end
 &nminitzm file='$INITZMFILE'                                  &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end

 &nmchck ocheck=t, ockall=f                             &end
 &nmbtdif tdmpc=0.0                                            &end

 &nmamat amatf='$MATFILS', omatv=t, omatd=f, omatt=t, omatp=f  &end
 &nmwfrc wvfrc='$EPFILS', fact=1.d0,1.d0,1.d0,1.d0, oep=t      &end

 &nmdata item='GRZ',    file='$DATZ'            &end

END_OF_DATA5
#
#  run
#
$RUN5 < $DIR/out/SYSIN >> $DIR/out/SYSOUT
echo job end at `date` >> $DIR/out/SYSOUT
#
cd $LNLIBD
rm -f PARA 
cat << END_OF_DATA6 >>! $LNLIBD/PARA
 &nmfin cfi='$MATFILS',
        cfo='$MATFEED',
        mwav=0,
        jrec=$irec,
        fact=$dfact
 &end
END_OF_DATA6
$RUN6 >> LOG
#
   endif
##############################
      @ iwav++
      end
   @ ilev++
   end
@ ivar++
end
#
cd $LNLIBD
rm -f PARA 
cat << END_OF_DATA7 >>! $LNLIBD/PARA
 &nmfin cfm='$AMATFIL',
        cfd='$MATFEED',
        cfo='$BMATFIL',
        mwav=0
 &end
END_OF_DATA7
$RUN7 >> LOG
#
echo ----------------
echo END EXECUTIONS
echo ----------------

