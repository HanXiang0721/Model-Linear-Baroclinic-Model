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


HAMP2=$(grep "hamp2" ./mk_2frc_bs.sh | awk -F = '{print $2}' | head -n 1)
XDIL2=$(grep "xdil2" ./mk_2frc_bs.sh | awk -F = '{print $2}' | head -n 1)
YDIL2=$(grep "ydil2" ./mk_2frc_bs.sh | awk -F = '{print $2}' | head -n 1)
XCNT2=$(grep "xcnt2" ./mk_2frc_bs.sh | awk -F = '{print $2}' | head -n 1)
YCNT2=$(grep "ycnt2" ./mk_2frc_bs.sh | awk -F = '{print $2}' | head -n 1)

VAMP2=$(grep "vamp2" ./mk_2frc_bs.sh | awk -F = '{print $2}' | head -n 1)
VCNT2=$(grep "vcnt2" ./mk_2frc_bs.sh | awk -F = '{print $2}' | head -n 1)
VDIL2=$(grep "vdil2" ./mk_2frc_bs.sh | awk -F = '{print $2}' | head -n 1)



sed -i "51s/^.*.$/XCNT=$XCNT/g" ncl_script/frc_horizontal_2.ncl
sed -i "52s/^.*.$/YCNT=$YCNT/g" ncl_script/frc_horizontal_2.ncl
sed -i "53s/^.*.$/XDIL=$XDIL/g" ncl_script/frc_horizontal_2.ncl
sed -i "54s/^.*.$/YDIL=$YDIL/g" ncl_script/frc_horizontal_2.ncl

sed -i "63s/^.*.$/XCNT2=$XCNT2/g" ncl_script/frc_horizontal_2.ncl
sed -i "64s/^.*.$/YCNT2=$YCNT2/g" ncl_script/frc_horizontal_2.ncl
sed -i "65s/^.*.$/XDIL2=$XDIL2/g" ncl_script/frc_horizontal_2.ncl
sed -i "66s/^.*.$/YDIL2=$YDIL2/g" ncl_script/frc_horizontal_2.ncl

ncl ncl_script/frc_horizontal_2.ncl





sed -i "10s/^.*.$/VAMP=$VAMP/g" ncl_script/frc_vertical_2.ncl
sed -i "11s/^.*.$/VDIL=$VDIL/g" ncl_script/frc_vertical_2.ncl
sed -i "12s/^.*.$/VCNT=$VCNT/g" ncl_script/frc_vertical_2.ncl

sed -i "31s/^.*.$/VAMP2=$VAMP2/g" ncl_script/frc_vertical_2.ncl
sed -i "32s/^.*.$/VDIL2=$VDIL2/g" ncl_script/frc_vertical_2.ncl
sed -i "33s/^.*.$/VCNT2=$VCNT2/g" ncl_script/frc_vertical_2.ncl

ncl ncl_script/frc_vertical_2.ncl

[[ ! -d "./pics" ]] && mkdir -p ./pics/frc

mv frc_*.pdf ./pics/frc

cd $DIR
