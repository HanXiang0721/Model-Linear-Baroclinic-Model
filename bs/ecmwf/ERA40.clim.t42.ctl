* T42 climatology by data/ECMWF/era40/era.intp.f   06/09/05
* ERA40 for 1961-1990
dset ^ERA40.clim.t42.grd
options sequential
undef -999.
title ERA15 T42 clim
XDEF 128 LINEAR 0. 2.81250
ydef 64 levels -87.864 -85.097 -82.313 -79.526 -76.737 -73.948 -71.158 
-68.368 -65.578 -62.787 -59.997 -57.207 -54.416 -51.626 -48.835 -46.045 
-43.254 -40.464 -37.673 -34.883 -32.092 -29.301 -26.511 -23.720 -20.930 
-18.139 -15.348 -12.558  -9.767  -6.976  -4.186  -1.395   1.395   4.186   
  6.976   9.767  12.558  15.348  18.139  20.930  23.720  26.511  29.301
 32.092  34.883  37.673  40.464  43.254  46.045  48.835  51.626  54.416 
 57.207  59.997  62.787  65.578  68.368  71.158  73.948  76.737  79.526 
 82.313  85.097  87.864 
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
