* PACKAGE ZL20C  !! vertical grid (CSIG20)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
      REAL*8     SIGML ( KMAX+1 )
      CHARACTER  HSIG  *(NCC)
      CHARACTER  HSIGM *(NCC)
*
      DATA       HSIG   / 'CSIG24'    /
      DATA       HSIGM  / 'CSIG24.M'  /
      DATA       SIGML  / 1.000, 0.990, 0.970, 0.930, 0.870,
     &                    0.790, 0.710, 0.630, 0.530, 0.430,
     &                    0.330, 0.240, 0.160, 0.100, 0.063,
     &                    0.040, 0.025, 0.016, 0.010, 0.0063, 
     &                    0.004, 0.0025,0.0016,0.0010,0.0    /
