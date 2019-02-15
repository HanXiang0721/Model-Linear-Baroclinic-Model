#!/bin/bash

rundays=30

kmo=10
navg=3

khpr=1
hamp=-1
xdil=10
ydil=10
xcnt=76   #135 #277 #300 #330   #135
ycnt=35    #15 #45  #70  #65   #40

kvpr=2
vamp=3.
vdil=20.
vcnt=0.8

ovor="f"
odiv="f"
otmp="t"
ops="f"
osph="f"

# clean all data
./clean.sh

# compile the model
./compile.csh

# make frc and bs file
sed -i "5s/^.*.$/kmo=$kmo/g" mk_frc_bs.sh
sed -i "6s/^.*.$/navg=$navg/g" mk_frc_bs.sh
sed -i "8s/^.*.$/khpr=$khpr/g" mk_frc_bs.sh
sed -i "9s/^.*.$/hamp=$hamp/g" mk_frc_bs.sh
sed -i "10s/^.*.$/xdil=$xdil/g" mk_frc_bs.sh
sed -i "11s/^.*.$/ydil=$ydil/g" mk_frc_bs.sh
sed -i "12s/^.*.$/xcnt=$xcnt/g" mk_frc_bs.sh
sed -i "13s/^.*.$/ycnt=$ycnt/g" mk_frc_bs.sh

sed -i "15s/^.*.$/kvpr=$kvpr/g" mk_frc_bs.sh
sed -i "16s/^.*.$/vamp=$vamp/g" mk_frc_bs.sh
sed -i "17s/^.*.$/vdil=$vdil/g" mk_frc_bs.sh
sed -i "18s/^.*.$/vcnt=$vcnt/g" mk_frc_bs.sh

sed -i "20s/^.*.$/ovor=$ovor/g" mk_frc_bs.sh
sed -i "21s/^.*.$/odiv=$odiv/g" mk_frc_bs.sh
sed -i "22s/^.*.$/otmp=$otmp/g" mk_frc_bs.sh
sed -i "23s/^.*.$/ops=$ops/g" mk_frc_bs.sh
sed -i "24s/^.*.$/osph=$osph/g" mk_frc_bs.sh

./mk_frc_bs.sh

# run!
sed -i "22s/^.*.$/setenv TEND     $rundays/g" t42-run.csh

./t42-run.csh

# plot frc file
./plot_frc.sh

# plot hgt 200 hPa
./plot_hgt.sh
