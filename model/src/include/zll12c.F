* PACKAGE ZL12C  !! vertical grid (CSIG12)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
      REAL*8     SIGML ( KMAX+1 )
      CHARACTER  HSIG  *(NCC)
      CHARACTER  HSIGM *(NCC)
*
      DATA       HSIG   / 'CSIG12'   /
      DATA       HSIGM  / 'CSIG12.M' /
      DATA       SIGML  / 1.000  , 0.990  , 0.970  , 0.930  , 0.850  ,
     &                    0.750  , 0.630  , 0.500  , 0.360  , 0.220  ,
     &                    0.100  , 0.050  , 0.000                    /
