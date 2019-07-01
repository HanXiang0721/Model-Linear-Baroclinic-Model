#!/bin/bash

./clean.sh

./compile.csh

./mk_2frc_bs.sh

./t42-run.csh

#./plot_frc_2.sh
#./plot_hgt.sh
cd ../out
./convert.sh
cd ../script/NA
mv ../../out/linear.t42l20.qqf.nc .
