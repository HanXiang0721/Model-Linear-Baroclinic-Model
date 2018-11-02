#!/bin/csh -f
#
#      storm track model
#      shell for Dec-alpha 
#      output is daily average
#
# NQS command for mail
#@$-q short
#
setenv LNHOME   /home/hiro/dvlop/ln_solver             # ROOT of model
setenv LBMDIR   $LNHOME/model                          # model directory
setenv SYSTEM   sun                                    # execute system
setenv RUN      $LBMDIR/bin/$SYSTEM/lbm2.t21ml11ctintgr # Excutable file
setenv DIR      $LNHOME/data/strack                    # Directory for Output
setenv TDIR     $LNHOME/solver/etc                     # tools, Gtool->GrADS
#
# 3D (win)
setenv DATZ     $LNHOME/bs/gt3/grz.t21                 # Topography
setenv DATS     $LNHOME/bs/gt3/sstwin                  # mean SST
setenv DATW     $LNHOME/bs/gt3/wgwin                   # soil wetness
#setenv DATI     $LNHOME/bs/gt3/gridx.t21               # sfc. index
setenv DATI     $LNHOME/data/strack/gridx.t21.mod      # sfc. index
setenv BSFILE   $LNHOME/bs/gt3/ncepwin.t21l11          # Atm. BS File
#
setenv RSTFILE  $DIR/Restart.amat                      # Restart-Data File
#setenv FRC      $LNHOME/data/strack/frc.random100.dry.grd # init pert.
#setenv FRC      $LNHOME/data/strack/frc.random100.moi.grd # init pert.
setenv FRC      $LNHOME/data/strack/frc.random200.moi.grd # init pert.
setenv SFRC     
setenv TRANS    $TDIR/outstr                           # executable
setenv CHECK    $TDIR/chkval                           # executable
setenv SFRC     $LNHOME/data/frc/frcsst.t21.midlat.grd # steady forcing
#
setenv OFIL1    $LNHOME/data/strack/out100.moi.grd
setenv OFIL2    $LNHOME/data/strack/eddy.stm.moi.grd
#
# tuning parameters
#
setenv ILNGTH   6                                      # length of integration
setenv NINTG    300                                    # No. of integration
##setenv NINTG    5                                      # No. of integration
setenv NINTV    1                                      # output inteval
#
#      loop for integration
#
rm -f $DIR/SYSOUT2
rm -f $OFIL1 $OFIL2
echo job start at `date`  > $DIR/SYSOUT2
#
@ iintg = 1
while ( $iintg <= $NINTG )
#
if (! -e $DIR) mkdir -p $DIR
cd $DIR
rm -f SYSOUT ERROUT SETPAR
echo job started at `date` > $DIR/SYSOUT
/bin/rm -f $DIR/SYSIN
#
#      parameters
#
# &nmsfcm owes=f, expw=0.5                                     &end
cat << END_OF_DATA >>! $DIR/SYSIN

 &nmrun  run='storm track model'                        &end
 &nmtime start=0,1,1,0,0,0, end=0,1,$ILNGTH,24,0,0      &end
 &nmdelt delt=40, tunit='MIN', inistp=2                 &end
 &nmhdif order=8, tefold=24, tunit='HOUR'               &end
 &nmdamp ddragv=0.5,1.,1.5,4.,4.,-1.,-1.,-1.,-1.,-1.,-1.,
         ddragd=0.5,1.,1.5,4.,4.,-1.,-1.,-1.,-1.,-1.,-1.,
         ddragt=2.5,3.5,4.5,8.,8.,-1.,-1.,-1.,-1.,-1.,-1.,
         ddragq=-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,
         tunit='DAY'                                           &end
 &nmldmp drglv= 1.5, 1.5, 1.5, 1.5, 1.5,-1.,-1.,-1.,-1.,-1.,-1.,
         drgld= 1.5, 1.5, 1.5, 1.5, 1.5,-1.,-1.,-1.,-1.,-1.,-1.,
         drglt= 1.5, 1.5, 1.5, 1.5, 1.5,-1.,-1.,-1.,-1.,-1.,-1.,
         drglq= 1.5, 1.5, 1.5, 1.5, 1.5,-1.,-1.,-1.,-1.,-1.,-1.,
         tuntl='DAY'                                           &end

 &nmvdif vdifv=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifd=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdift=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,
         vdifq=1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3,1.d3  &end

 &nmmca ocum=t, ttau0=2.5, qtau0=2.5, ttauc=5, tunit='HOUR', 
        sigkb=0.9D0, flnd=1.d0                               &end
 &nmlsc olsc=t, ttaul0=1.5, qtaul0=1.5, tunit='HOUR', 
        dqrat=1.d-2, flnd=1.d0                                &end
 &nmsfcm gfdtmx=2.5, gfdtmn=1.0, expw=1., owes=t             &end

 &nminit file='$BSFILE' , DTBFR=0., DTAFTR=0., TUNIT='DAY' &end
 &nmrstr file='$RSTFILE', tintv=1, tunit='MON', DFMT='UR4', overwt=t &end

 &nmchck ocheck=f, ockall=f                             &end

 &nmfrc  ffrc='$FRC',   oper=t, nfcs=$iintg             &end
 &nmsfrc fsfrc='$SFRC', ofrc=f, osstf=t, nsfcs=1        &end

 &nmdata item='GRZ',    file='$DATZ'                    &end
 &nmdata item='GRSST',  file='$DATS'                    &end
 &nmdata item='GRIDX',  file='$DATI'                    &end
 &nmdata item='GRWG',   file='$DATW'                    &end

 &nmhisd tintv=1, tavrg=3, tunit='HOUR'                 &end
 &nmhist item='PSI',file='s',  tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='VOR',file='r',  tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='U',  file='u',  tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='V',  file='v',  tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='T',  file='t',  tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='Z',  file='z',  tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='PS', file='p',  tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='Q',  file='q',  tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='Q1C',file='dtc',tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='Q2C',file='dqc',tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='Q1L',file='dtl',tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='Q2L',file='dql',tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
 &nmhist item='PR', file='pr', tintv=$NINTV,tavrg=$NINTV,tunit='DAY' &end
END_OF_DATA
#
#  run
#
( $RUN < $DIR/SYSIN >> $DIR/SYSOUT ) >& $DIR/ERROUT
echo job end at `date` >> $DIR/SYSOUT
#
#      parameter for trans
#
cat << END_TRANS >>! $DIR/SETPAR
 &nmrec nfcs=$iintg, nfday=2, nlday=$ILNGTH, tint=$NINTV, tunit='DAY' &end
 &nmfili cfs='$DIR/s',
         cfr='$DIR/r',
         cfu='$DIR/u',
         cfv='$DIR/v',
         cft='$DIR/t',
         cfz='$DIR/z',
         cfp='$DIR/p',
         cfq='$DIR/q',
         cfx='$DIR/dtc',
         cfy='$DIR/dqc',
         cfx2='$DIR/dtl',
         cfy2='$DIR/dql',
         cfpr='$DIR/pr',
 &end
 &nmfilo cfo='$OFIL1',
         cfo2='$OFIL2',
         cbs='$LNHOME/bs/grads/ncepwin.t21l11.grd',
         opl=f,
         ofo=t
 &end
 &nmcls  oclassic=f
 &end
END_TRANS
$TRANS < $DIR/SETPAR >> $DIR/SYSOUT2
#
   @ iintg++ 
end
#
#      parameter for check
#
rm -f $DIR/SETPAR
cd $DIR
cat << END_CHECK >>! $DIR/SETPAR
 &nmrec nfcs=$NINTG, nfday=2, nlday=$ILNGTH, tint=$NINTV, tunit='DAY'  &end
 &nmfil cfo='$LNHOME/data/strack/out1000.grd',
        cfoc='$LNHOME/data/strack/out1000.c.grd',
 &end
END_CHECK
##$CHECK < $DIR/SETPAR >> $DIR/SYSOUT2
echo job end at `date` >> $DIR/SYSOUT2

