* sample file for T21L8 response (full moist LBM)
DSET ^rsp.t21l8.grd
* BYTESWAPPED
OPTIONS SEQUENTIAL YREV
TITLE dumy
UNDEF -999.
XDEF 64 LINEAR 0. 5.625
YDEF 32 LEVELS -85.761 -80.269 -74.745 -69.213 -63.679 -58.143 -52.607 
-47.070 -41.532 -35.995 -30.458 -24.920 -19.382 -13.844 -8.3067 -2.7689 
2.7689 8.3067 13.844 19.382 24.920 30.458 35.995 41.532 47.070 52.607 
58.143 63.679 69.213 74.745 80.269 85.761
ZDEF 8  LEVELS 1000 950 850 700 500 250 100 50
TDEF 1 LINEAR 15jan0000 1mo
VARS 6
v      8 99 stream function   response   [m**2 s**-1]
d      8 99 velocity potentialresponse   [m**2 s**-1]
t      8 99 temperature response         [K]
z      8 99 geopotential height response [m]
p      1 99 surface pressure response    [hPa]
q      8 99 specific humidity response   [kg/kg] 
ENDVARS
