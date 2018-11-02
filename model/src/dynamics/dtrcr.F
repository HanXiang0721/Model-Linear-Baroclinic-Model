* PACKAGE DTRCR !! dynamics: tracer transport
*   [HIS] 95/05/28(numaguti) AGCM5.4.03
**********************************************************************
      SUBROUTINE TRACER    !! tracer transport
     M         ( GAQ   , GBQ   ,
     I           GAU   , GAV   , GAPS  , GADIV , GSIGD ,
     I           DELT  , OINIT , DIFQ  , LT    ,
     W           GTUQ  , GTVQ  , GTQ   , WDQ   , WTQ    )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM                  */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physics etc.           */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY] 
      REAL*8     GAQ   ( IJDIM, KMAX   )      !! humidity etc.
*        : grid value(t) <GENGD>  grid value(t+dt)
*
      REAL*8     GBQ   ( IJDIM, KMAX   )      !! humidity etc.
*        : grid value(t-dt) <DADVNC>  grid value(t)
*
*   [INPUT] 
      REAL*8     GAU   ( IJDIM, KMAX   )     !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX   )     !! southern wind v
      REAL*8     GAPS  ( IJDIM )             !! surface pressure
      REAL*8     GADIV ( IJDIM, KMAX   )     !! divergence D
      REAL*8     GSIGD ( IJDIM, KMAX+1 )     !! divergence D
      REAL*8     DELT                        !! time step
      LOGICAL    OINIT                       !! initialized condition ?
      REAL*8     DIFQ  ( NMDIM, KMAX  )      !! h-diff. coef. of water
      INTEGER    LT                          !! tracer No.
*
*   [WORK] 
      REAL*8     GTUQ  ( IJDIM, KMAX   )  !! tend. zonal  adv.
      REAL*8     GTVQ  ( IJDIM, KMAX   )  !! tend. merid. adv.
      REAL*8     GTQ   ( IJDIM, KMAX   )  !! tend.
      REAL*8     WDQ   ( NMDIM, KMAX   )  !! wave
      REAL*8     WTQ   ( NMDIM, KMAX   )  !! wave tendency
*
*   [INTERNAL PARM] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DTRCR: DYNAMICS TRACER EQ. 96/05/28'
         OFIRST = .FALSE.
      ENDIF
*
      CALL GRQADV                      !!  adv. of humidity etc.
     O         ( GTUQ  , GTVQ  , GTQ   ,
     I           GAQ   ,
     I           GAU   , GAV   , GADIV , GSIGD , LT     )
*
      CALL HCONVW
     O         ( WTQ   ,
     M           GTUQ  , GTVQ  , 
     I           KMAX            )
*
      CALL G2W
     M         ( WTQ   ,
     I           GTQ   , '    ', 'ADDO', KMAX )
*
      CALL G2W
     O         ( WDQ   ,
     I           GBQ   , '    ', 'POS' , KMAX )
*
      CALL TRINTG         !! time integration (spectral)
     M         ( WDQ   , 
     I           WTQ   , 
     I           DELT  , DIFQ  , LT    )
*
      IF ( .NOT. OINIT ) THEN
         CALL QADVNC      !! time advancing + time filter
     M         ( GBQ   ,
     I           GAQ     )
      ENDIF
*
      CALL W2G            !! generate new grid point value
     O         ( GAQ   ,
     I           WDQ   , '    ', 'POS ', KMAX    )
*      
      RETURN
      END
