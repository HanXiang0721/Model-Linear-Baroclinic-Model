* PACKAGE ZSDIM  !! size of saving information
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
*
      INTEGER    NGSBUF, MGSBUF, MGSBFK, MGSBF1
      PARAMETER (
#ifndef OPT_MGSBFK
     &           MGSBFK= 5+NCRF*5 ,
#else
     &           MGSBFK= OPT_MGSBUF ,
#endif
#ifndef OPT_MGSBF1
     &           MGSBF1= 5 ,
#else
     &           MGSBF1= OPT_MGSBF1 ,
#endif
#ifndef OPT_MGSBUF
     &           MGSBUF= MGSBFK+MGSBF1 ,
#else
     &           MGSBUF= OPT_MGSBUF ,
#endif
#ifndef OPT_NGSBUF
     &           NGSBUF=IJDIM*(KMAX+1)*MGSBFK
     &                 +IJDIM*MGSBF1 
#else
     &           NGSBUF= OPT_NGSBUF
#endif
     &           ) 

