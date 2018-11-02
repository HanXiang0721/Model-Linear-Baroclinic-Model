* PACKAGE ZMDIM  !! No.of mozaic grid point
*
*         97/08/04(numaguti) AGCM5.4h
*
      INTEGER     IMOZ           !! No.of surface i-mosaic
      INTEGER     JMOZ           !! No.of surface j-mosaic
      INTEGER     NMOZ           !! No.of surface total mosaic
*
      PARAMETER (
#ifndef OPT_IMOZ
     &            IMOZ   = 1,
#else
     &            IMOZ   = OPT_IMOZ,
#endif
#ifndef OPT_JMOZ
     &            JMOZ   = 1,
#else
     &            JMOZ   = OPT_JMOZ,
#endif
#ifndef OPT_NMOZ
     &            NMOZ   = IMOZ*JMOZ
#else
     &            NMOZ   = OPT_NMOZ
#endif
     &          )
