* eddy statistics in a storm track model (STM)
DSET ^eddy.stm.grd
OPTIONS SEQUENTIAL YREV
TITLE strom track model
UNDEF -999.
XDEF 64 LINEAR 0. 5.625
YDEF 32 LEVELS -85.761 -80.269 -74.745 -69.213 -63.679 -58.143 -52.607 
-47.070 -41.532 -35.995 -30.458 -24.920 -19.382 -13.844 -8.3067 -2.7689 
2.7689 8.3067 13.844 19.382 24.920 30.458 35.995 41.532 47.070 52.607 
58.143 63.679 69.213 74.745 80.269 85.761
ZDEF 11 LEVELS 0.9950 0.9800 0.9499 0.8999 0.8146 
0.6789 0.5133 0.3478 0.2025 0.09234 0.02075
TDEF 1  LINEAR 15JAN0000 1MO
VARS 10
uu    11   99  squared transient zonal wind (U'**2) [m^2/s^2]
vv    11   99  squared transient meridional wind (V'**2) [m^2/s^2]
tt    11   99  squared transient temperature (T'**2) [K^2]
zz    11   99  squared transient height (Z'**2) [m^2]
tke   11   99  kinetic energy for eddies (U'**2+V'**2)/2 [m^2/s^2]
uv    11   99  eddy momentum flux (U'V') [m^2/s^2]
ut    11   99  eddy zonal heat flux (U'T') [mK/s]
vt    11   99  eddy meridional heat flux (V'T') [mK/s]
ur    11   99  eddy zonal vorticity flux (U'ZETA') [10^-5m/s^2]
vr    11   99  eddy meridional vorticity flux (V'ZETA') [10^-5m/s^2]
ENDVARS
