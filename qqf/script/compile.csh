#!/bin/csh -f
#
#     batch shell for linear matrix solver
#
# NQS command for mail
#@$-q   q
#@$-N   1
#@$-me
#
setenv LNHOME /home/qqf/Documents/Linear-Baroclinic-Model            # ROOT of model
setenv DIRM   $LNHOME/model/src  # ROOT of executable
setenv DIR    $LNHOME/solver     # ROOT of executable
setenv MYHOME $LNHOME/qqf/script
setenv LOG    $MYHOME/compile.log
#
#      model
#
cd $DIRM
rm -f $LOG
echo compilation started at `date` >& $LOG
#
#make clean
make lib lbm >>& $LOG
#
#make clean.special
#make lbm >> $LOG
#
#      solver
#
cd $DIR
cd util/
make clean >>& $LOG
make all >>& $LOG

cd $MYHOME
