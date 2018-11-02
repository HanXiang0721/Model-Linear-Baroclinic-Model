#!/bin/csh -f
#
#     batch shell for linear matrix solver
#
# NQS command for mail
#@$-q   q
#@$-N   1
#@$-me
#
setenv LNHOME /var/opt/ln_solver            # ROOT of model
setenv DIRM   /var/opt/ln_solver/model/src  # ROOT of executable
setenv DIR    /var/opt/ln_solver/solver     # ROOT of executable
setenv LOG    $LNHOME/model/sh/message.comp
#
#      model
#
cd $DIRM
rm -f $LOG
echo compilation started at `date` > $LOG
#
#make clean
make lib lbm >> $LOG
#
#make clean.special
#make lbm >> $LOG
#
#      solver
#
cd $DIR
#cd util/
#make clean
#make >> $LOG
#cd ../steady/
#make clean
#make >> $LOG
#cd ../custom/
#make clean
#make >> $LOG
#cd ../expert/
#make clean
#make >> $LOG
#cd ../cg/
#make clean
#make >> $LOG
#cd ../aim/
#make clean
#make >> $LOG
#make br >> $LOG
