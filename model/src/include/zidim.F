* PACKAGE ZIDIM  !! size of history file
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/19(numaguti) AGCM5.4f
*         98/07/25(hiro) AGCM5.4g
*
*
      INTEGER     NHISGK
      INTEGER     NHISG1
      INTEGER     NHISZK
      INTEGER     NHISZ1
      INTEGER     NHISZ0
      INTEGER     NHIST
      INTEGER     NHISR
      INTEGER     NHDIM
      INTEGER     KPDIM                !! p-level for output
      INTEGER     NAXWRK
      INTEGER     NAXMAX
      INTEGER     NCOMAX
      INTEGER     NGDWRK
      INTEGER     NWRKO         !" work space for level ocean
*
      PARAMETER ( NHISGK= 30, NHISG1 = 30,
     &            NHISZK= 20, NHISZ1 = 20, NHISZ0 = 30,
#ifndef OPT_NHIST 
     &            NHIST = NHISGK + NHISG1 + 
     &                    NHISZK + NHISZ1 + NHISZ0,
#else
     &            NHIST = OPT_NHIST ,
#endif
#ifndef OPT_NHISR
     &            NHISR = 400,
#else      
     &            NHISR = OPT_NHISR ,
#endif
#ifndef OPT_NHDIM
     &            NHDIM = NHISGK*IMAX*JMAX*KMAX
     &                  + NHISG1*IMAX*JMAX
     &                  + NHISZK*JMAX*KMAX
     &                  + NHISZ1*JMAX
     &                  + NHISZ0,
#else      
     &            NHDIM = OPT_NHDIM ,
#endif
#ifndef OPT_KPDIM
     &            KPDIM  = 18,
#else
     &            KPDIM  = OPT_KPDIM,
#endif
#ifndef OPT_NAXWRK
     &            NAXWRK = 512,
#else      
     &            NAXWRK = OPT_NAXWRK,
#endif
#ifndef OPT_NAXMAX
     &            NAXMAX = 12,
#else      
     &            NAXMAX = OPT_NAXMAX,
#endif
#ifndef OPT_NCOMAX
     &            NCOMAX = 10,
#else      
     &            NCOMAX = OPT_NCOMAX,
#endif
     &            NWRKO = 200000, 
     &            NGDWRK = IJDIM*(KMAX+KPDIM) + NWRKO )
