* basic state
* derived from summer mean fields in NCEP reanalysis
DSET ^ncepsum.t42l20.grd
* BYTESWAPPED
OPTIONS SEQUENTIAL YREV
TITLE BASIC STATE
UNDEF -999.
XDEF 128 LINEAR 0. 2.8125
YDEF  64 LEVELS 
 -87.8638 -85.0965 -82.3129 -79.5256 -76.7369 -73.9475 -71.1577
 -68.3678 -65.5776 -62.7873 -59.9970 -57.2066 -54.4162 -51.6257
 -48.8352 -46.0447 -43.2542 -40.4636 -37.6731 -34.8825 -32.0919
 -29.3014 -26.5108 -23.7202 -20.9296 -18.1390 -15.3484 -12.5578
 -9.76715 -6.97653 -4.18592 -1.39531 1.39531 4.18592 6.97653
  9.76715 12.5578 15.3484 18.1390 20.9296 23.7202 26.5108
  29.3014 32.0919 34.8825 37.6731 40.4636 43.2542 46.0447
  48.8352 51.6257 54.4162 57.2066 59.9970 62.7873 65.5776
  68.3678 71.1577 73.9475 76.7369 79.5256 82.3129 85.0965
  87.8638
ZDEF 20  LEVELS 0.99500 0.97999 0.94995 0.89988 0.82977 0.74468 
0.64954 0.54946 0.45447 0.36948 0.29450 0.22953 0.17457 0.12440 
0.0846830 0.0598005 0.0449337 0.0349146 0.0248800 0.00829901
TDEF 1 LINEAR 15jan0000 1mo
VARS 5
u      20 99 zonal wind        [m/s]
v      20 99 meridional wind   [m/s]
t      20 99 temperature       [K]
p      1  99 sfc. pressure     [hPa]
q      20 99 specific humidity [kg/kg]
ENDVARS
