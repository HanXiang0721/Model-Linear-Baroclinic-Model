* NCEP climatology interporated into T10 Gaussian grid
*
dset ^ncep.clim.y58-97.t10.grd
OPTIONS SEQUENTIAL
undef 9.999E+20
title NCEP cdas climatology (T10)
XDEF 32  LINEAR 0. 11.250
YDEF 16  LEVELS  -81.651  -70.835  -59.955  -49.061  -38.161  -27.259
-16.356  -5.4520  5.4520  16.356 27.259  38.161  49.061 
59.955  70.835   81.651
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
