* T21 climatology by data/ECMWF/era40/era.intp.f   06/09/05
* ERA40 for 1961-1990
dset ^ERA40.clim.t21.grd
options sequential
undef -999.
title ERA15 T21 clim
XDEF 64 LINEAR 0. 5.625
YDEF 32 LEVELS -85.761 -80.269 -74.745 -69.213 -63.679 -58.143 -52.607 
-47.070 -41.532 -35.995 -30.458 -24.920 -19.382 -13.844 -8.3067 -2.7689 
2.7689 8.3067 13.844 19.382 24.920 30.458 35.995 41.532 47.070 52.607 
58.143 63.679 69.213 74.745 80.269 85.761
zdef    23 levels 1000 925 850 775 700 600 500 400 300 250 200 150 100 70 50 30 20 10 7 5 3 2 1
tdef    12 linear 00Z01jan61 1mon
vars 6
u        23   0  U-component of Wind [m/s]
v        23   0  V-component of Wind [m/s]
t        23   0  Temperature         [K]
z        23   0  Geopotential height [m]
q        23   0  Specific humidity   [kg/kg]
slp       0   0  Mean sea level pressure [hPa]
endvars
