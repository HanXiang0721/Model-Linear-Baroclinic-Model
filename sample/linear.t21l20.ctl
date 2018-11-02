* sample file fot products of a linear integration
* moist LBM
DSET ^linear.t21l20.grd
OPTIONS SEQUENTIAL YREV
TITLE time-integration
UNDEF -999.
XDEF 64 LINEAR 0. 5.625
YDEF 32 LEVELS -85.761 -80.269 -74.745 -69.213 -63.679 -58.143 -52.607 
-47.070 -41.532 -35.995 -30.458 -24.920 -19.382 -13.844 -8.3067 -2.7689 
2.7689 8.3067 13.844 19.382 24.920 30.458 35.995 41.532 47.070 52.607 
58.143 63.679 69.213 74.745 80.269 85.761
ZDEF 20  LEVELS 1000 950 900 850 700 600 500 400 300 250 
200 150 100  70  50 30  20  10   7   5
TDEF 30 LINEAR 15jan0000 1dy
VARS 14
psi    20 99 stream function     [m**2/s]
chi    20 99 velocity potential  [m**2/s]
u      20 99 zonal wind          [m/s]
v      20 99 meridional wind     [m/s]
w      20 99 p-vertical velocity [hPa/s]
t      20 99 temperature         [K]
z      20 99 geopotential height [m]
p       1 99 surface pressure    [hPa]
q      20 99 specific humidity   [kg/kg]
dtc    20 99 convective heat source Q1      [K/s]
dqc    20 99 convective moisture source -Q2 [kg/kg/s]
dtl    20 99 large-scale heat source Q1     [K/s]
dql    20 99 large-scale moisture source -Q2[kg/kg/s]
pr      0 99 precipitation                  [mm/day]
ENDVARS
