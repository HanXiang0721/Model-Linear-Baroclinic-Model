* basic state
* derived from winter mean fields in NCEP reanalysis
DSET ^ncepwin.t21l8.grd
* BYTESWAPPED
OPTIONS SEQUENTIAL YREV
TITLE BASIC STATE
UNDEF -999.
XDEF 64 LINEAR 0. 5.625
YDEF 32 LEVELS -85.761 -80.269 -74.745 -69.213 -63.679 -58.143 -52.607 
-47.070 -41.532 -35.995 -30.458 -24.920 -19.382 -13.844 -8.3067 -2.7689 
2.7689 8.3067 13.844 19.382 24.920 30.458 35.995 41.532 47.070 52.607 
58.143 63.679 69.213 74.745 80.269 85.761
ZDEF 8  LEVELS  0.995 0.945 0.830 0.653 0.463 0.303 0.162 0.0415
TDEF 1 LINEAR 15jan0000 1mo
VARS 5
u      8 99 zonal wind        [m/s]
v      8 99 meridional wind   [m/s]
t      8 99 temperature       [K]
p      1 99 sfc. pressure     [hPa]
q      8 99 specific humidity [kg/kg]
ENDVARS
