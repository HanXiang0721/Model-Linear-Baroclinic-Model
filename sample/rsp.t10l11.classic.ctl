* test response pattern (full matrix)
DSET ^rsp.t10l11.classic.grd
* BYTESWAPPED
OPTIONS SEQUENTIAL YREV
TITLE Linear Response
UNDEF -999.
XDEF 32  LINEAR 0.  11.250
YDEF 16  LEVELS  -81.651  -70.835  -59.955  -49.061  -38.161  -27.259
-16.356  -5.4520  5.4520  16.356 27.259  38.161  49.061 
59.955  70.835   81.651
ZDEF 11 LEVELS 1000 950 900 850 700 500 300 200 100 30 10
TDEF 1 LINEAR 15jan0000 1MO
VARS 5
u     11 99 u-wind   response       [m/s]
v     11 99 v-wind   response       [m/s]
t     11 99 temperature             [K]
z     11 99 geopotential height     [m]
p      1 99 surface pressure        [hPa]
ENDVARS
