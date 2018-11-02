#!/bin/sh

#rm -rf *.grd
#
if [[ ! -e "ncep.nc" ]]; then
	ncl preprocess-ncep2.ncl
fi

#gfortran -I/usr/include -L/usr/lib/x86_64-linux-gnu -l netcdff  create_lbm_bs_T21.f90 

#./a.out

#rm -rf a.out

gfortran -I/usr/include -L/usr/lib/x86_64-linux-gnu -l netcdff  create_lbm_bs_T42.f90

./a.out

rm -rf a.out
