* PACKAGE ZL20C  !! vertical grid (CSIG20)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
      REAL*8     SIGML ( KMAX+1 )
      CHARACTER  HSIG  *(NCC)
      CHARACTER  HSIGM *(NCC)
*
      DATA       HSIG   / 'CSIG20'    /
      DATA       HSIGM  / 'CSIG20.M'  /
      DATA       SIGML  / 1.000, 0.990, 0.970, 0.930, 0.870,
     &                    0.790, 0.700, 0.600, 0.500, 0.410,
     &                    0.330, 0.260, 0.200, 0.150, 0.100,
     &                    0.070, 0.050, 0.040, 0.030, 0.020, 
     &                    0.0  /
