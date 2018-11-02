* PACKAGE ZL5C  !! vertical grid (CSIG5)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
      REAL*8     SIGML ( KMAX+1 )
      CHARACTER  HSIG  *(NCC)
      CHARACTER  HSIGM *(NCC)
*
      DATA       HSIG   / 'ESIG5'   /
      DATA       HSIGM  / 'ESIG5.M'  /
      DATA       SIGML  / 1.000, 0.800, 0.600, 0.400,
     &                    0.200, 0.000                /
