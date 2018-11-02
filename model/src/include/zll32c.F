* PACKAGE ZL27C  !! vertical grid (CSIG32)
*
*   [HIS] 96/05/27(numaguti) AGCM5.4.01
*
      REAL*8     SIGML ( KMAX+1 )
      CHARACTER  HSIG  *(NCC)
      CHARACTER  HSIGM *(NCC)
*
      DATA       HSIG   / 'CSIG32'    /
      DATA       HSIGM  / 'CSIG32.M'  /
      DATA       SIGML  / 1.000, 0.995, 0.990, 0.980, 0.970,
     &                    0.960, 0.945, 0.930, 0.910, 0.890,
     &                    0.860, 0.830, 0.790, 0.740, 0.680,
     &                    0.600, 0.500, 0.410, 0.330, 0.260,
     &                    0.200, 0.150, 0.120, 0.100, 0.080,
     &                    0.070, 0.060, 0.050, 0.040, 0.030,
     &                    0.020, 0.010, 0.000                /


