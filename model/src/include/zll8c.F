* PACKAGE ZLL8C  !! vertical grid (CSIG8)
*
*   [HIS] 01/11/13(hiro)
*
      REAL*8     SIGML ( KMAX+1 )
      CHARACTER  HSIG  *(NCC)
      CHARACTER  HSIGM *(NCC)
*
      DATA       HSIG   / 'CSIG8'   /
      DATA       HSIGM  / 'CSIG8.M' /
      DATA       SIGML  / 1.000  , 0.990  , 0.90   , 0.76   , 0.550  ,
     &                    0.380  , 0.230  , 0.10   , 0.000  /
