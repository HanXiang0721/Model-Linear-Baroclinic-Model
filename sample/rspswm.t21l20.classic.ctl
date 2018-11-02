* test response pattern (SWM, T21L20m10)
* TDEF is dummy for zonal wave number
DSET ^rspswm.t21l20.classic.grd
* BYTESWAPPED
TITLE dumy
UNDEF -999.
XDEF 64 LINEAR 0. 5.625
YDEF 32 LEVELS -85.761 -80.269 -74.745 -69.213 -63.679 -58.143 -52.607 
-47.070 -41.532 -35.995 -30.458 -24.920 -19.382 -13.844 -8.3067 -2.7689 
2.7689 8.3067 13.844 19.382 24.920 30.458 35.995 41.532 47.070 52.607 
58.143 63.679 69.213 74.745 80.269 85.761
ZDEF 20  LEVELS 1000 950 900 850 700 600 500 400 300 250 
200 150 100  70  50 30  20  10   7   5
TDEF 11 LINEAR 15jan0000 1mo
VARS 5
u      20 99 u-wind response [m/s]
v      20 99 v-wind response [m/s]
t      20 99 temp.  response [K]
z      20 99 height response [m]
p      1 99 Ps     response [hPa]
ENDVARS
