* PACKAGE ZL5C  !! vertical grid (CSIG5)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
      REAL*8     SIGML ( KMAX+1 )
      CHARACTER  HSIG  *(NCC)
      CHARACTER  HSIGM *(NCC)
*
      DATA       HSIG   / 'CSIG5'   /
      DATA       HSIGM  / 'CSIG5.M'  /
      DATA       SIGML  / 1.000, 0.800, 0.600, 0.300,
     &                    0.150, 0.000                /
