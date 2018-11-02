* sample eigenvalues (barotropic)
DSET ^evalbaro.grd
* x=1: real eigen value
* x=2: imaginary eigen value
OPTIONS SEQUENTIAL
TITLE dumy
UNDEF -999.
XDEF 2    LINEAR  0 1
YDEF 1    LINEAR  0 1
ZDEF 483  LINEAR  483. -1.
TDEF 1    LINEAR 15jan0001 1yr
VARS 1
e     483 99 eigen value
ENDVARS
