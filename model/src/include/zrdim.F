* PACKAGE ZRDIM  !! size of irdat file
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/07/09(numaguti) AGCM5.4f
*         98/07/25(hiro) AGCM5.4g
*
      INTEGER     NRDAT
      INTEGER     NRDIM
      INTEGER     NRITM
      INTEGER     NWRKO         !" work space for level ocean
      PARAMETER  (
     &            NWRKO = 200000, 
#ifndef OPT_NRDAT
     &            NRDAT = 12,
#else      
     &            NRDAT = OPT_NRDAT ,
#endif
#ifndef OPT_NRDIM
     &            NRDIM = NRDAT*IJDIM+JDIM*KDIM + NWRKO,
#else      
     &            NRDIM = OPT_NRDIM, 
#endif
#ifndef OPT_NRITM
     &            NRITM = NRDAT*3
#else      
     &            NRITM = OPT_NRITM
#endif
     &            )
