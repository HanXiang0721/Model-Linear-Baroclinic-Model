* sample forcing pattern
DSET ^frc.t10l11.classic.grd
* BYTESWAPPED
TITLE dumy
UNDEF -999.
XDEF 32  LINEAR 0. 11.250
YDEF 16  LEVELS  -81.651  -70.835  -59.955  -49.061  -38.161  -27.259
-16.356  -5.4520  5.4520  16.356 27.259  38.161  49.061 
59.955  70.835   81.651
ZDEF 11  LEVELS 0.9950 0.9800 0.9499 0.8999 0.8146 
0.6789 0.5133 0.3478 0.2025 0.09234 0.02075
TDEF 1 LINEAR 15jan0000 1mo
VARS 4
v     11 99 vor.   forcing [s**-2]
d     11 99 div.   forcing [s**-2]
t     11 99 temp.  forcing [K s**-1]
p      1 99 sfc.Ln(Ps) forcing
ENDVARS
