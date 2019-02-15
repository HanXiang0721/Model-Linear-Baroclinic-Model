#!/bin/bash

DIR=`pwd`

kmo=10
navg=3

khpr=1
hamp=0.00001
xdil=15
ydil=8
xcnt=115
ycnt=20

kvpr=2
vamp=1.
vdil=10.
vcnt=0.3

ovor=t
odiv=f
otmp=f
ops=f
osph=f

cd ../../solver/util/

[[ -f SETPAR ]] && rm -rf SETPAR

cat > ./SETPAR  << SETPAR 
 &nmfgt cfs='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/psi',
        cfc='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/chi',
        cfu='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/u',
        cfv='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/v',
        cfw='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/w',
        cft='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/t',
        cfz='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/z',
        cfp='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/p',
        cfq='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/q',
        cfx='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/dt',
        cfy='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/Gtools/dq',
        cfo='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/out/linear.t42l20.qqf.grd',
        fact=1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
        opl=t,
 &end

 &nmbs  cbs0='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/bs/qqf.t42l20',
        cbs='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/bs/qqf.t42l20.grd'
 &end

 &nmncp cncep='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/mybs/ncep.clim.y79-14.t42.grd',
        cncep2='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/mybs/ncep.clim.y79-14.ps.t42.grd',
        calt='/home/qqf/Documents/Linear-Baroclinic-Model/bs/gt3/grz.t42',
        kmo=$kmo, navg=$navg, ozm=f, osw=f, ousez=t
 &end

 &nmecm cecm='/home/qqf/Documents/Linear-Baroclinic-Model/bs/ecmwf/ERA40.clim.t42.grd',
        calt='/home/qqf/Documents/Linear-Baroclinic-Model/bs/gt3/grz.t42',
        kmo=6, navg=3, ozm=f, osw=f
 &end

 &nmfin cfm='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/frc/frc.t42l20.CNP.mat',
        cfg='/home/qqf/Documents/Linear-Baroclinic-Model/qqf/frc/frc.t42l20.CNP.grd'
        fact=1.0,1.0,1.0,1.0,1.0
 &end

 &nmvar ovor=$ovor, odiv=$odiv, otmp=$otmp, ops=$ops, osph=$osph
 &end

 &nmhpr khpr=$khpr,
        hamp=$hamp,
        xdil=$xdil,
        ydil=$ydil,
        xcnt=$xcnt,
        ycnt=$ycnt
 &end

 &nmvpr kvpr=$kvpr,
        vamp=$vamp,
        vdil=$vdil,
        vcnt=$vcnt
 &end

 &nmall owall=t
 &end

 &nmcls oclassic=t
 &end


 &nmred cdr='/home/qqf/Documents/Linear-Baroclinic-Model/matrix.moi',
        cfo='/home/qqf/Documents/Linear-Baroclinic-Model/matrix.moi/mat/MAT.t21l11m6.ncepannzm.moi.tmp.dat'
 &end
SETPAR

./ncepsbs >& /dev/null
./mkfrcng >& /dev/null

cd $DIR
