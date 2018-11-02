* NCEP climatology interporated into T21 Gaussian grid
*
dset ^ncep.clim.y58-97.t21.grd
OPTIONS SEQUENTIAL
undef 9.999E+20
title NCEP cdas climatology (T21)
XDEF 64 LINEAR 0. 5.625
YDEF 32 LEVELS -85.761 -80.269 -74.745 -69.213 -63.679 -58.143 -52.607 
-47.070 -41.532 -35.995 -30.458 -24.920 -19.382 -13.844 -8.3067 -2.7689 
2.7689 8.3067 13.844 19.382 24.920 30.458 35.995 41.532 47.070 52.607 
58.143 63.679 69.213 74.745 80.269 85.761
tdef      12 linear 00Z01jan58 1mo
zdef 17 levels
1000 925 850 700 600 500 400 300 250 200 150 100 70 50 30 20 10
vars      7
z    17 99 Geopotential height [gpm]
rh    8 99 Relative humidity [%]
q     8 99 Specific humidity [kg/kg]
t    17 99 Temperature [K]
u    17 99 zonal wind [m/s]
v    17 99 meridional wind [m/s]
omg  12 99 Pressure vertical velocity [Pa/s]
endvars
