#!/bin/bash

DIR=`pwd`

cd ../out

rm -rf linear.t42l20.qqf.nc

cdo -f nc import_binary linear.t42l20.qqf.ctl linear.t42l20.qqf.nc

cd $DIR

[[ ! -d "./pics/HGT" ]] && mkdir -p ./pics/HGT

ncl ncl_script/plot_hgt_200.ncl
ncl ncl_script/plot_hgt_850.ncl
ncl ncl_script/plot_t_200.ncl

