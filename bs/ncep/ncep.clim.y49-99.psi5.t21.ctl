* NCEP/NCAR reanalysis climatology during 1949-99
DSET  ^ncep.clim.y49-99.psi5.t21.grd
OPTIONS SEQUENTIAL
TITLE NCEP reanalysis climatology
UNDEF -9.99E+33
XDEF 64 LINEAR 0. 5.625
YDEF 32 LEVELS -85.761 -80.269 -74.745 -69.213 -63.679 -58.143 -52.607 
               -47.070 -41.532 -35.995 -30.458 -24.920 -19.382 -13.844 
                -8.3067 -2.7689  2.7689  8.3067 13.844  19.382  24.920
                30.458  35.995  41.532  47.070  52.607  58.143  63.679
                69.213  74.745  80.269  85.761
ZDEF 1   LEVELS 500
TDEF 12  LINEAR 15JAN1949 1MO
VARS 1
psi    1 99 stream function at 500 hPa
ENDVARS
