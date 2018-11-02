* PACKAGE ZLL11C  !! vertical grid (CSIG11)
*
*   [HIS] 95/05/24(numaguti) 
*
      REAL*8     SIGML ( KMAX+1 )
      CHARACTER  HSIG  *(NCC)
      CHARACTER  HSIGM *(NCC)
*
      DATA       HSIG   / 'CSIG11'   /
      DATA       HSIGM  / 'CSIG11.M' /
      DATA       SIGML  / 1.000  , 0.990  , 0.970  , 0.930  , 
     &                    0.870  , 0.760  , 0.600  , 0.430  ,
     &                    0.270  , 0.140  , 0.050  , 0.000    /
