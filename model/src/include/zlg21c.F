* PACKAGE ZLG21C  !! surface vertical grid(GLEVC2,WLEVC1)
*
*   [HIS] 95/06/24(numaguti)
*   [HIS] 95/06/21(numaguti) AGCM5.4f
*
      REAL*8     GLEVML ( KGMAX+1 )
      CHARACTER  HGLEV  *(NCC)
      CHARACTER  HGLEVM *(NCC)
*
      REAL*8     WLEVML ( KWMAX+1 )
      CHARACTER  HWLEV  *(NCC)
      CHARACTER  HWLEVM *(NCC)
*
      DATA       HGLEV  / 'GLEVC2'   /
      DATA       HGLEVM / 'GLEVC2.M' /
      DATA       GLEVML / 0.000  , 0.050  , 1.000 /
*
      DATA       HWLEV  / 'WLEVC1'   /
      DATA       HWLEVM / 'WLEVC1.M' /
      DATA       WLEVML / 0.000  , 1.000 /
