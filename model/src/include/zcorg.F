* PACKAGE ZCORD  !! standard coordinate (common)
*
*   [HIS] 96/06/19(numaguti) AGCM5.4f
*
      REAL*8     GLEV   ( 0:KGMAX   )          !! level of ground
      REAL*8     GLEVM  ( 0:KGMAX+1 )          !! level of ground
      REAL*8     DGLV   ( 0:KGMAX   )          !! land thickness
      REAL*8     DGLVM  ( 0:KGMAX+1 )          !! land thickness
      REAL*8     WLEV   ( KWMAX   )            !! level of ground
      REAL*8     WLEVM  ( KWMAX+1 )            !! level of ground
      REAL*8     DWLV   ( KWMAX   )            !! land thickness
      REAL*8     DWLVM  ( KWMAX+1 )            !! land thickness
      COMMON    /COMCOG/
     &           GLEV, GLEVM, DGLV, DGLVM,
     &           WLEV, WLEVM, DWLV, DWLVM
