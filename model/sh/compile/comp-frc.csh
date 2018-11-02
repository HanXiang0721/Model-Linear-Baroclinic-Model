#!/bin/csh -f
#
#     batch shell for linear matrix solver
#
# NQS command for mail
#@$-q   b
#@$-N   1
#@$-me
#
setenv LNHOME /home/q16655/ln_solver            # ROOT of model
setenv DIRM   /home/q16655/ln_solver/model/src  # ROOT of executable
setenv LOG    $LNHOME/model/sh/message.comp
#
#      model
#
cd $DIRM
rm -f $LOG
make clean.special
make lbm > $LOG
