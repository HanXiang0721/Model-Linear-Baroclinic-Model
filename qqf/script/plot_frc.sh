#!/bin/bash

DIR=`pwd`

HAMP=$(grep "hamp" ../../solver/util/SETPAR | awk -F = '{print $2}' | sed -e "s/,//g")
XDIL=$(grep "xdil" ../../solver/util/SETPAR | awk -F = '{print $2}' | sed -e "s/,//g")
YDIL=$(grep "ydil" ../../solver/util/SETPAR | awk -F = '{print $2}' | sed -e "s/,//g")
XCNT=$(grep "xcnt" ../../solver/util/SETPAR | awk -F = '{print $2}' | sed -e "s/,//g")
YCNT=$(grep "ycnt" ../../solver/util/SETPAR | awk -F = '{print $2}' | sed -e "s/,//g")

VAMP=$(grep "vamp" ../../solver/util/SETPAR | awk -F = '{print $2}' | sed -e "s/,//g")
VCNT=$(grep "vcnt" ../../solver/util/SETPAR | awk -F = '{print $2}' | sed -e "s/,//g")
VDIL=$(grep "vdil" ../../solver/util/SETPAR | awk -F = '{print $2}' | sed -e "s/,//g")

sed -i "51s/^.*.$/XCNT=$XCNT/g" ncl_script/frc_horizontal.ncl
sed -i "52s/^.*.$/YCNT=$YCNT/g" ncl_script/frc_horizontal.ncl
sed -i "53s/^.*.$/XDIL=$XDIL/g" ncl_script/frc_horizontal.ncl
sed -i "54s/^.*.$/YDIL=$YDIL/g" ncl_script/frc_horizontal.ncl

ncl ncl_script/frc_horizontal.ncl

sed -i "10s/^.*.$/VAMP=$VAMP/g" ncl_script/frc_vertical.ncl
sed -i "11s/^.*.$/VDIL=$VDIL/g" ncl_script/frc_vertical.ncl
sed -i "12s/^.*.$/VCNT=$VCNT/g" ncl_script/frc_vertical.ncl

ncl ncl_script/frc_vertical.ncl

[[ ! -d "./pics" ]] && mkdir -p ./pics/frc

mv frc_horizontal.pdf frc_vertical.pdf ./pics/frc

cd $DIR
