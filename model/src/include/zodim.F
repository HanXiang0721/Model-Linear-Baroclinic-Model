* PACKAGE ZODIM !! No. of grid point for ocean
*
*   [HIS] 97/08/07(numaguti) AGCM5.4g
*
*
      INTEGER     IODIV          !! grid division of ocean (lon)
      INTEGER     JODIV          !! grid division of ocean (lat)
      INTEGER     IOMAX          !! size of longitude dimension
      INTEGER     JOMAX          !! size of latitude dimension
      INTEGER     IODIM          !! size of longitude dimension
      INTEGER     JODIM          !! size of latitude dimension
      INTEGER     IJODIM         !! IDIM*JDIM
*
      PARAMETER ( 
#ifdef OPT_DOUBLE_OCEAN
     &            IODIV  = 1         ,  JODIV  = 1,
#else
#ifdef OPT_IODIV
     &            IODIV =  OPT_IODIV ,
#else
     &            IODIV =  IMOZ      ,
#endif
#ifdef OPT_JODIV
     &            JODIV =  OPT_JODIV ,
#else
     &            JODIV =  JMOZ      ,
#endif
#endif
     &            IOMAX  = IMAX*IODIV ,  JOMAX  = JDIM*JODIV,
     &            IODIM  = IDIM*IODIV ,  JODIM  = JDIM*JODIV,
     &            IJODIM = IODIM*JODIM                       )
