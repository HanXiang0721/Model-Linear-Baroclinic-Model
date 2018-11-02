* PACKAGE DVECT      !! dynamics: vector operation
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*   [HIS] 95/08/03(numaguti) DIV(K=0), V(K=0)=0 when LMAX=0
*   [HIS] 96/06/25(numaguti) AGCM5.4f
***********************************************************************
      SUBROUTINE UV2VDW    !! (U,V) -> (vor.,D) spectral
     O         ( WDVOR , WDDIV ,
     I           GDU   , GDV   , OVOR  , ODIV  ,
     W           GDUU  , GDVV                    )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM                  */
#include        "zccom.F"                /* ER                     */
      INTEGER    KDIMD
*
*   [OUTPUT]
      REAL*8     WDVOR ( NMDIM , KMAX )  !! vorticity
      REAL*8     WDDIV ( NMDIM , KMAX )  !! divergence D
*
*   [INPUT]
      REAL*8     GDU   ( IJDIM, KMAX  )  !! westerly u
      REAL*8     GDV   ( IJDIM, KMAX  )  !! southern wind v
      LOGICAL    OVOR                    !! rotation ?
      LOGICAL    ODIV                    !! divergent ?
*
*   [ENTRY OUTPUT] 
      REAL*8     WCNV  ( NMDIM, KDIMD )  !! horizontal convergence
      REAL*8     GDDX  ( IJDIM, KDIMD )  !! zonal differential
      REAL*8     GDDY  ( IJDIM, KDIMD )  !! meridional differential
*
*   [ENTRY INPUT] 
      REAL*8     GFX   ( IJDIM, KDIMD )  !! zonal flux
      REAL*8     GFY   ( IJDIM, KDIMD )  !! meridional flux
      REAL*8     GD    ( IJDIM, KDIMD )
*
*   [WORK] 
      REAL*8     GDUU  ( IJDIM, KMAX  )  !! westerly U
      REAL*8     GDVV  ( IJDIM, KMAX  )  !! southern wind V
      REAL*8     WDPSI ( NMDIM, KMAX  )  !! psi
      REAL*8     WDCHI ( NMDIM, KMAX  )  !! chi
      REAL*8     WD    ( NMDIM, KDIMD )
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, NM
*
*   [INTERNAL SAVE] 
      REAL*8     UVFACT( IJDIM  )        !! factor of u -> U
      REAL*8     EDEL  ( NMDIM  )        !! vor.,D -> U,V
      SAVE       UVFACT, EDEL
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DVECT: VECTOR OPERATIONS 96/06/25'
         OFIRST = .FALSE.
         CALL   DSETUV
     O        ( UVFACT )
         CALL   DSETED
     O        ( EDEL   )
      ENDIF
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GDUU ( IJ,K ) = GDU ( IJ,K ) * UVFACT( IJ )
            GDVV ( IJ,K ) = GDV ( IJ,K ) * UVFACT( IJ )
 1100 CONTINUE
*
      IF ( OVOR ) THEN
         CALL G2W
     O         ( WDVOR ,
     I           GDUU  ,
     I           'YGRA', 'POS ', KMAX )
*
         IF ( MMAX .GE. 1 ) THEN
            CALL G2W
     O         ( WDVOR ,
     I           GDVV  ,
     I           'XGRA', 'ADD ', KMAX )
         ENDIF
      ENDIF
*
      IF ( ODIV ) THEN
         IF ( LMAX .GE. 1 ) THEN
            CALL G2W
     O         ( WDDIV ,
     I           GDVV  ,
     F           'YGRA', 'NEG ', KMAX )
         ELSE
            CALL RESET( WDDIV, NMDIM*KMAX )
         ENDIF
*
         IF ( MMAX .GE. 1 ) THEN
            CALL G2W
     O         ( WDDIV ,
     I           GDUU  ,
     F           'XGRA', 'ADD ', KMAX )
         ENDIF
      ENDIF
*
      RETURN
*===================================================================
      ENTRY      VDW2UV    !! (vor.,D)spectral -> (U,V)
     O         ( GDU   , GDV   ,
     I           WDVOR , WDDIV , OVOR  , ODIV  ,
     W           WDPSI , WDCHI                   )
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DVECT: U,V -> VOR,DIV 96/06/25'
         OFIRST = .FALSE.
         CALL   DSETUV
     O        ( UVFACT )
         CALL   DSETED
     O        ( EDEL   )
      ENDIF
*
      IF ( OVOR ) THEN
         DO 2100 K = 1, KMAX
            DO 2100 NM = 1, NMDIM
               WDPSI( NM,K ) = WDVOR( NM,K ) * EDEL( NM )
 2100    CONTINUE
         CALL W2G
     O         ( GDU   ,
     I           WDPSI ,
     I           'YGRA', 'NEG ', KMAX )
         IF ( MMAX .GE. 1 ) THEN
            CALL W2G
     M         ( GDV   ,
     I           WDPSI ,
     I           'XGRA', 'POS ', KMAX )
         ELSE
            CALL RESET( GDV, IJDIM*KMAX )
         ENDIF
      ELSE
         CALL  RESET( GDU, IJDIM*KMAX )
         CALL  RESET( GDV, IJDIM*KMAX )
      ENDIF
*
      IF ( ODIV ) THEN
         DO 2200 K = 1, KMAX
            DO 2200 NM = 1, NMDIM
               WDCHI( NM,K ) = WDDIV( NM,K ) * EDEL( NM )
 2200    CONTINUE
         IF ( MMAX .GE. 1 ) THEN
            CALL W2G
     M         ( GDU   ,
     I           WDCHI ,
     I           'XGRA', 'ADD ', KMAX )
         ENDIF
         IF ( LMAX .GE. 1 ) THEN
            CALL W2G
     O         ( GDV   ,
     I           WDCHI ,
     I           'YGRA', 'ADD ', KMAX )
         ENDIF
      ENDIF
*
      DO 3100 K = 1, KMAX
         DO 3100 IJ = 1, IJDIM
            GDU ( IJ,K ) = GDU ( IJ,K ) / UVFACT( IJ )
            GDV ( IJ,K ) = GDV ( IJ,K ) / UVFACT( IJ )
 3100 CONTINUE
*
      RETURN
*==================================================================
      ENTRY      HCONVW         !! horizontal convergence
     O         ( WCNV  ,
     M           GFX   , GFY   , 
     I           KDIMD           )
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DVECT: U,V -> VOR,DIV 96/06/25'
         OFIRST = .FALSE.
         CALL   DSETUV
     O        ( UVFACT )
         CALL   DSETED
     O        ( EDEL   )
      ENDIF
*
      DO 4100 K = 1, KDIMD
         DO 4100 IJ = 1, IJDIM
            GFX ( IJ,K ) = GFX ( IJ,K ) * UVFACT( IJ )
            GFY ( IJ,K ) = GFY ( IJ,K ) * UVFACT( IJ )
 4100 CONTINUE 
*
      IF ( LMAX .GE. 1 ) THEN
         CALL G2W
     O         ( WCNV  ,
     I           GFY   ,
     I           'YGRA', 'POS ', KDIMD )
      ELSE
         CALL RESET( WCNV, NMDIM*KDIMD )
      ENDIF
*
      IF ( MMAX .GE. 1 ) THEN
         CALL G2W
     M         ( WCNV  ,
     I           GFX   ,
     I           'XGRA', 'SUB ', KDIMD )
      ENDIF
*
      RETURN
*=====================================================================
      ENTRY      HGRAD     !! horizontal gradient
     O         ( GDDX  , GDDY  ,
     I           GD    , KDIMD ,
     W           WD             )
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DVECT: U,V -> VOR,DIV 96/06/25'
         OFIRST = .FALSE.
         CALL   DSETUV
     O        ( UVFACT )
         CALL   DSETED
     O        ( EDEL   )
      ENDIF
*
      CALL G2W
     O         ( WD   ,
     I           GD   ,
     I           '    ', 'POSO', KDIMD )
*
      IF ( MMAX .GE. 1 ) THEN
         CALL W2G
     O         ( GDDX ,
     I           WD   ,
     I           'XGRA', 'POSO', KDIMD )
      ELSE
         CALL RESET ( GDDX , IJDIM*KDIMD )
      ENDIF
*
      IF ( LMAX .GE. 1 ) THEN
         CALL W2G
     O         ( GDDY ,
     I           WD   ,
     I           'YGRA', 'POSO', KDIMD )
      ELSE
         CALL RESET( GDDY, IJDIM*KDIMD )
      ENDIF
*
      DO 5100 K = 1, KDIMD
         DO 5100 IJ = 1, IJDIM
            GDDX ( IJ,K ) = GDDX ( IJ,K ) / UVFACT ( IJ ) / ER
            GDDY ( IJ,K ) = GDDY ( IJ,K ) / UVFACT ( IJ ) / ER
 5100 CONTINUE 
*
      RETURN
      END
***********************************************************************
      SUBROUTINE UV2VDG    !! (U,V) -> (vor.,D) grid
     O         ( GDVOR , GDDIV ,
     I           GDU   , GDV   , OVOR  , ODIV  ,
     W           GDX   , GDY   , WDVOR , WDDIV  )
*
*   [PARAM] 
#include        "zcdim.F"
#include        "zddim.F"                /* NMDIM */
#include        "zccom.F"
*
*   [OUTPUT]
      REAL*8     GDVOR ( IJDIM , KMAX )  !! vorticity
      REAL*8     GDDIV ( IJDIM , KMAX )  !! divergence D
*
*   [INPUT]
      REAL*8     GDU   ( IJDIM, KMAX )   !! westerly u
      REAL*8     GDV   ( IJDIM, KMAX )   !! southern wind v
      LOGICAL    OVOR                    !! rotation ?
      LOGICAL    ODIV                    !! divergent ?
*
*   [WORK] 
      REAL*8     GDX   ( IJDIM, KMAX )  !! work, psi, chi
      REAL*8     GDY   ( IJDIM, KMAX )  !! work
      REAL*8     WDVOR ( NMDIM, KMAX ) 
      REAL*8     WDDIV ( NMDIM, KMAX )
*
*   [INTERNAL WORK] 
      INTEGER    K, NM
      LOGICAL    OOUT
*
*   [INTERNAL SAVE] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      REAL*8     EDEL  ( NMDIM  )            !! vor.,D -> U,V
      LOGICAL    OFIRST
      SAVE
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL   DSETNM
     O        ( NMO    )
         CALL   DSETED
     O        ( EDEL   )
      ENDIF
*
      CALL UV2VDW    !! (U,V) -> (vor.,D)spectral
     O         ( WDVOR , WDDIV ,
     I           GDU   , GDV   , OVOR  , ODIV  ,
     W           GDX   , GDY                     )
*
      IF ( OVOR ) THEN
         DO 1100 K = 1, KMAX
            WDVOR( NMO(1,0,0),K ) = 0.D0
 1100    CONTINUE 
*
         CALL W2G
     O         ( GDVOR ,
     I           WDVOR ,
     I           '    ', 'POS ', KMAX )
         CALL HISTRG
     I    ( OOUT, 'PSI', 'stream function   ' ,'m**2/s', 'ALEV'   )
         IF ( OOUT ) THEN
            DO 2100 K = 1, KMAX
               DO 2100 NM = 1, NMDIM
                  WDVOR( NM,K ) = WDVOR( NM,K ) * EDEL( NM ) * ER 
 2100       CONTINUE
            CALL W2G
     O         ( GDX   ,
     I           WDVOR ,
     I           '    ', 'POS ', KMAX )
            CALL HISTAX( GDX, 'PSI', 1.D0, .FALSE. )         
         ENDIF
      ENDIF
*
      IF ( ODIV ) THEN
         DO 1200 K = 1, KMAX
            WDDIV( NMO(1,0,0),K ) = 0.D0
 1200    CONTINUE 
*
         CALL W2G
     O         ( GDDIV ,
     I           WDDIV ,
     I           '    ', 'POS ', KMAX )
         CALL HISTRG
     I    ( OOUT, 'CHI', 'velocity potential' ,'m**2/s', 'ALEV'   )
         IF ( OOUT ) THEN
            DO 2200 K = 1, KMAX
               DO 2200 NM = 1, NMDIM
                  WDDIV( NM,K ) = WDDIV( NM,K ) * EDEL( NM ) * ER 
 2200       CONTINUE
            CALL W2G
     O         ( GDX   ,
     I           WDDIV ,
     I           '    ', 'POS ', KMAX )
            CALL HISTAX( GDX, 'CHI', 1.D0, .FALSE. )         
         ENDIF
      ENDIF
*
      RETURN
*======================================================================
      ENTRY      VD2UVG    !! (vor.,D) -> (U,V) grid
     O         ( GDU   , GDV   ,
     I           GDVOR , GDDIV , OVOR  , ODIV  ,
     W           WDVOR , WDDIV                   )
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL   DSETNM
     O        ( NMO    )
         CALL   DSETED
     O        ( EDEL   )
      ENDIF
*
      IF ( OVOR ) THEN
         CALL G2W
     O         ( WDVOR ,
     I           GDVOR , '    ', 'POSO' , KMAX    )
      ENDIF
      IF ( ODIV ) THEN
         CALL G2W
     O         ( WDDIV ,
     I           GDDIV , '    ', 'POSO' , KMAX    )
      ENDIF

      CALL VDW2UV         !! (vor.,D)spectral -> (U,V)
     O         ( GDU   , GDV   ,
     I           WDVOR , WDDIV , OVOR  , ODIV  ,
     W           WDVOR , WDDIV                    )
*
      RETURN
      END
