* PACKAGE ZQDIM  !! No.of grid point etc. of physics etc.
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/27(numaguti) AGCM5.4f
*         96/09/09(numaguti) river
*         96/10/21(numaguti) ZQDIM
*
      INTEGER     KFMAX          !! Max level of frozen ground water
      INTEGER     KSNMAX         !! Max level of snow
      INTEGER     KWSMAX         !! Max number of surface water
*
      INTEGER     KGSFC          !! index of surface skin temperature
      INTEGER     KGG            !! index of ground temperature
      INTEGER     KGDIM          !! No.of layer of ground + SKIN (heat)
      INTEGER     KWW            !! index of soil water
      INTEGER     KWFRS          !! index of frozen ground
      INTEGER     KWSNW          !! index of snow mass
      INTEGER     KWSFC          !! index of surface (intercepted) water
      INTEGER     KWDIM          !! total No.of layer of ground(water
*
      INTEGER     ITL            !! cloud water
      INTEGER     NTR            !! humidity etc., No. of tracers
      INTEGER     NCRF           !! No.of radiation calculation
      INTEGER     NRALB          !! No. of band for albedo
      INTEGER     IDXMIN         !! min. surface index
      INTEGER     IDXMAX         !! max. surface index
      INTEGER     IDXNUM         !! No.of surface index
*
#include        "zmdim.F"                /* mosaic */
*
#ifdef OPT_FROST
      PARAMETER ( KFMAX  = KWMAX )
#else
      PARAMETER ( KFMAX  = 0     )
#endif

#ifdef OPT_KSNMAX
      PARAMETER ( KSNMAX = OPT_KSNMAX )
#else
      PARAMETER ( KSNMAX = 1     )
#endif

#ifdef OPT_KWSMAX 
      PARAMETER ( KWSMAX = OPT_KWSMAX )
#else
# ifdef OPT_INTERCEP
      PARAMETER ( KWSMAX = 1          )
# else
      PARAMETER ( KWSMAX = 0          )
# endif
#endif
*
      PARAMETER ( 
     &            KGSFC  = 1,
     &            KGG    = 2,
     &            KGDIM  = KGMAX+1, 
     &            KWW    = 1,
     &            KWFRS  = KWMAX+1,
     &            KWSFC  = KWFRS+KFMAX,
     &            KWSNW  = KWSFC+KWSMAX,
     &            KWDIM  = KWSNW+KSNMAX-1 )
*
      PARAMETER (
#ifdef  OPT_NOQL
     &            ITL    = 1,
     &            NTR    = 1
#else
#ifndef OPT_ITL
CC     &            ITL    = 2,
     &            ITL    = 1,
#else
     &            ITL    = OPT_ITL ,
#endif
#ifndef OPT_NTR
CC     &            NTR    = 2
     &            NTR    = 1
#else
     &            NTR    = OPT_NTR 
#endif
#endif
     &                               )
*
      PARAMETER (
#ifdef OPT_NOCRF
     &            NCRF   = 1, 
#else
     &            NCRF   = 2,      
#endif
#ifndef OPT_NRALB
     &            NRALB  = 3, 
#else
     &            NRALB  = OPT_NRALB,      
#endif
#ifndef OPT_IDXMIN
     &            IDXMIN = -1,      
#else
     &            IDXMIN = OPT_IDXMIN ,
#endif
#ifndef OPT_IDXMAX
     &            IDXMAX = 32,      
#else
     &            IDXMAX = OPT_IDXMAX ,
#endif
     &            IDXNUM = IDXMAX-IDXMIN+1 )

