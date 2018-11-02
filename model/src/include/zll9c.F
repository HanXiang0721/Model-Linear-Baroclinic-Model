* PACKAGE ZLL9C  !! vertical grid (CSIG9)
*
*   [HIS] 95/05/18(numaguti)
*
      REAL*8     SIGML ( KMAX+1 )
      CHARACTER  HSIG  *(NCC)
      CHARACTER  HSIGM *(NCC)
*
      DATA       HSIG   / 'CSIG9'   /
      DATA       HSIGM  / 'CSIG9.M' /
      DATA       SIGML  / 1.000  , 0.980  , 0.900  , 0.760  , 0.600  ,
     &                    0.430  , 0.270  , 0.140  , 0.050  , 0.000  /
