#!/bin/bash

DIR=`pwd`

rm -rf compile.log
rm -rf pics

rm -rf ../bs/qqf.t21l20 ../bs/qqf.t21l20.grd
rm -rf ../frc/frc.t21l20.CNP.grd  ../frc/frc.t21l20.CNP.mat  ../frc/frc.t21l20.CNP.nc ../frc/*.eps ../*.pdf
rm -rf ../Gtools/*
rm -rf ../out/linear.t21l20.qqf.grd ../out/linear.t21l20.qqf.nc

rm -rf ../bs/qqf.t42l20 ../bs/qqf.t42l20.grd
rm -rf ../frc/frc.t42l20.CNP.grd  ../frc/frc.t42l20.CNP.mat  ../frc/frc.t42l20.CNP.nc ../frc/*.eps ../frc/*.pdf
rm -rf ../Gtools/*
rm -rf ../out/linear.t42l20.qqf.grd ../out/linear.t42l20.qqf.nc

cd ../../solver/util/
make clean >& /dev/null

cd $DIR

cd ../../model/src
make clean >& /dev/null
make clean.special >& /dev/null

cd $DIR
