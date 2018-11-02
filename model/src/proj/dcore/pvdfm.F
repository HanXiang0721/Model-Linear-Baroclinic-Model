* PACKAGE PVDFM  !! physics: vertical diffusion
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/13(numaguti) unit of QFLUX
*         96/10/24(numaguti) default of FMRIB1
*         96/11/19(numaguti) FMRIB
*         96/11/21(numaguti) DELTP
*         96/12/16(numaguti) SEDIM
***********************************************************************
      SUBROUTINE VDFFLX    !! vertical diffusion
     O         ( UFLUX , VFLUX , DUFLUX,        
     I           GDU   , GDV   , 
     I           GDT   , GDTV  , GDTVM , 
     I           GDP   , GDPM  , GDZ   , GDZM   )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     UFLUX ( IJSDIM, KMAX+1       ) !! flux of U
      REAL*8     VFLUX ( IJSDIM, KMAX+1       ) !! flux of V
      REAL*8     DUFLUX( IJSDIM, KMAX+1, -1:0 ) !! impl. mat. of U
*
*   [INPUT] 
      REAL*8     GDU   ( IJSDIM, KMAX      )   !! westerly U
      REAL*8     GDV   ( IJSDIM, KMAX      )   !! southern wind V
      REAL*8     GDT   ( IJSDIM, KMAX      )   !! temperature T
      REAL*8     GDTV  ( IJSDIM, KMAX      )   !! virtual temperature
      REAL*8     GDTVM ( IJSDIM, KMAX+1    )   !! temp. (half lev)
      REAL*8     GDP   ( IJSDIM, KMAX      )   !! pressure P
      REAL*8     GDPM  ( IJSDIM, KMAX      )   !! pressure (half lev)
      REAL*8     GDZ   ( IJSDIM, KMAX      )   !! altitude
      REAL*8     GDZM  ( IJSDIM, KMAX      )   !! altitude (half lev)
*
*   [INTERNAL WORK] 
#ifndef OPT_NOCOMMON
      COMMON 
     &           DVDZ  , RIB   ,
     &           DFM   , 
     &           EXNER , EXNERM, GQS   ,
     &           WORK1 , WORK2 , WORK3 
#endif
      REAL*8     DVDZ  ( IJSDIM, KMAX )   !! d|v|/dz
      REAL*8     RIB   ( IJSDIM, KMAX )   !! bulk Ri number
      REAL*8     DFM   ( IJSDIM, KMAX )   !! trans. coef. :u
      REAL*8     EXNER ( IJSDIM, KMAX )   !! Exner function : integer
      REAL*8     EXNERM( IJSDIM, KMAX )   !! Exner func.: half lev
      REAL*8     GQS   ( IJSDIM, KMAX )   !! saturation humidity & work 
      REAL*8     WORK1 ( IJSDIM, KMAX )   !! work
      REAL*8     WORK2 ( IJSDIM, KMAX )
      REAL*8     WORK3 ( IJSDIM, KMAX )
*
      INTEGER    IJ, K, LT, KK
      INTEGER    IFPAR, JFPAR
      REAL*8     AKAPPA, FRI, THVK, THVKM, FC
      REAL*8     RIBD, RIBS, RIBX
      REAL*8     RHO, RDZ
*
*   [INTERNAL PARM] 
      REAL*8     P00, THS
      REAL*8     URMIN , RIBMIN, RIBMAX
      REAL*8     FMRIB0, FMRIB1, FMRIB , RIBSMN
      LOGICAL    OFIRST
      NAMELIST  /NMVDFM/ THS   , URMIN , RIBMIN, RIBMAX, 
     &                   FMRIB0, FMRIB1, FMRIB , RIBSMN
*
*   [INTERNAL FUNC]
#include        "zqsat.F"                /* saturate vapor pressure */
      DATA       P00    / 100000. /
      DATA       THS    / 300.    /           !! basic pot. temp
      DATA       URMIN  / 0.1     /           !! min. of del-wind sq.
      DATA       RIBMIN / -100.   /           !! min. of bulk Ri number
      DATA       RIBMAX /  100.   /           !! max. of bulk Ri number
      DATA       FMRIB0 / 0.0     /           !! moist Ri factor
      DATA       FMRIB1 / 0.5     /           !! moist Ri factor
      DATA       RIBSMN /  0.     /           !! min. of wet bulk Ri number
      DATA       OFIRST / .TRUE.  /
*
*          < 1. constant >
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PVDFM: VERTICAL DIFFUSION 93/12/22'
         OFIRST = .FALSE.
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMVDFM, END=1190 )
 1190    WRITE  ( JFPAR, NMVDFM )
      ENDIF
*
      AKAPPA = RAIR / CP
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJSDIM
            EXNER ( IJ,K ) = ( GDP ( IJ,K )/P00 ) ** AKAPPA
            EXNERM( IJ,K ) = ( GDPM( IJ,K )/P00 ) ** AKAPPA
            GQS   ( IJ,K ) = FQSAT( GDT( IJ,K ), GDP( IJ,K ) )
 1100 CONTINUE
*
      CALL RESET( DVDZ, IJSDIM )
      CALL RESET( RIB , IJSDIM )
*
      DO 2100 K = 2, KMAX
         DO 2100 IJ = 1, IJSDIM
            DVDZ( IJ,K ) = SQRT( MAX( URMIN,
     &                               ( GDU( IJ,K )-GDU( IJ,K-1 ) )**2
     &                              +( GDV( IJ,K )-GDV( IJ,K-1 ) )**2
     &                         )    )
     &                       / ( GDZ( IJ,K ) - GDZ( IJ,K-1 ) )
 2100 CONTINUE
*
      DO 2200 K = 2, KMAX
         DO 2200 IJ = 1, IJSDIM
            FRI   = GRAV / THS / ( GDZ( IJ,K ) - GDZ( IJ,K-1 ) )
     &                         / DVDZ ( IJ,K )**2
*
            THVK  = GDTV( IJ,K   )/EXNER( IJ,K   )
            THVKM = GDTV( IJ,K-1 )/EXNER( IJ,K-1 )
            RIBD  = FRI * ( THVK - THVKM )
*
            THVK  = THVK  + EL/CP*GQS( IJ,K   )/EXNER( IJ,K   )
            THVKM = THVKM + EL/CP*GQS( IJ,K-1 )/EXNER( IJ,K-1 )
            RIBS  = FRI * ( THVK - THVKM )
            RIBS  = MAX( RIBS, RIBSMN )
*
CX            FC    = FMRIB1*GDCFRC( IJ,K-1 ) + FMRIB0*GDCFRC( IJ,K )
CX            FC    = MIN( MAX( FC, 0.D0 ), 1.D0 )
            FC = 0.D0
*
            RIBX  = RIBD + FC * MIN( RIBS - RIBD, 0.D0 )
            RIB  ( IJ,K ) = MIN( MAX ( RIBX, RIBMIN ), RIBMAX )
            WORK1( IJ,K ) = RIBD
            WORK2( IJ,K ) = RIBS
 2200 CONTINUE
*
      CALL HISTIN
     I   ( WORK1, 'RIBD' , 'bulk Ri No. DRY' ,'N.D.', 'ALEV')
      CALL HISTIN
     I   ( WORK2, 'RIBS' , 'bulk Ri No. WET' ,'N.D.', 'ALEV')
      CALL HISTIN
     I   ( RIB,   'RIB'  , 'bulk Ri No.' ,'N.D.', 'ALEV')
*
      CALL VDFCOF
     O         ( DFM   , 
     I           RIB   , DVDZ  , GDZM  ,
     W           WORK1 , WORK2 , WORK3 , GQS   )
*
      CALL HISTIN
     I   ( DFM, 'DFM'  , 'diffusion coef. momentum' ,'m**2/s', 'ALEV')
*
*          < 4. transfer coef. >
*
      DO 4100 K = 2, KMAX
         DO 4100 IJ = 1, IJSDIM
            RHO         = GDPM( IJ,K ) / RAIR / GDTVM( IJ,K )
            RDZ         = RHO /( GDZ( IJ,K ) - GDZ( IJ,K-1 ) )
            DFM( IJ,K ) = RDZ * DFM( IJ,K )
 4100 CONTINUE
*
*          < 5. fluxes >
*
      DO 5100 K = 2, KMAX
         DO 5100 IJ = 1, IJSDIM
            UFLUX( IJ,K ) = DFM( IJ,K )
     &                       * ( GDU ( IJ,K-1 ) - GDU ( IJ,K ) )
            VFLUX( IJ,K ) = DFM( IJ,K )
     &                       * ( GDV ( IJ,K-1 ) - GDV ( IJ,K ) )
 5100 CONTINUE
*
*          < 6. implicit matrix >
*
      DO 6100 K = 2, KMAX
         DO 6100 IJ = 1, IJSDIM
            DUFLUX( IJ,K, 0 ) = -      DFM( IJ,K )
            DUFLUX( IJ,K,-1 ) =        DFM( IJ,K )
 6100 CONTINUE
*
      DO 6300 K = 1, KMAX+1, KMAX
         DO 6300 KK = -1, 0
            CALL RESET( DUFLUX(1,K,KK), IJSDIM )
 6300 CONTINUE 
*
      RETURN
      END
***********************************************************************
      SUBROUTINE VDFCOF    !! vertical diffusion coefficient
     O         ( DFM   , 
     I           RIB   , DVDZ  , GDZM  ,
     W           RIF   , SHT   , SMT   , AML    )
*
*      Ymamada Mellor, 1982
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     DFM   ( IJSDIM, KMAX )   !! trans. coef. :u
*
*   [INPUT] 
      REAL*8     RIB   ( IJSDIM, KMAX )   !! bulk Ri number
      REAL*8     DVDZ  ( IJSDIM, KMAX )   !! d|v|/dz
      REAL*8     GDZM  ( IJSDIM, KMAX )   !! altitude
*
*   [WORK] 
      REAL*8     RIF   ( IJSDIM, KMAX )   !! flux Ri number
      REAL*8     SHT   ( IJSDIM, KMAX )   !! tilde(Sh)
      REAL*8     SMT   ( IJSDIM, KMAX )   !! tilde(Sm)
      REAL*8     AML   ( IJSDIM, KMAX )   !! mixing length
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
      INTEGER    IFPAR, JFPAR
      REAL*8     FRZ, DFX, ZF, ZFCT
*
*   [INTERNAL SAVE] 
      REAL*8     ALP1, ALP2
      REAL*8     BET1, BET2, BET3, BET4
      REAL*8     GAM1, GAM2
      REAL*8     FAC1, FAC2, FAC3
      REAL*8     RIFC
      SAVE       ALP1, ALP2, BET1, BET2, BET3, BET4,
     &           GAM1, GAM2, FAC1, FAC2, FAC3, RIFC
*
*   [INTERNAL PARM] 
      REAL*8     AML0
      REAL*8     SHTMIN, SMTMIN
      REAL*8     RIFMIN, RIFMAX
      REAL*8     DFMMIN, DFMMAX
      REAL*8     FZDF  , ZDF0  , ZDFE  
      REAL*8     A1    , B1    , A2    , B2    , C1
      LOGICAL    OFIRST
      NAMELIST  /NMVDFY/ AML0  ,
     &                   SHTMIN, SMTMIN, 
     &                   RIFMIN, RIFMAX,
     &                   DFMMIN, DFMMAX,
     &                   FZDF  , ZDF0  , ZDFE  ,
     &                   A1    , B1    , A2    , B2    , C1
      DATA       AML0   /   300.   /         !! max. mixing length 
      DATA       RIFMIN /  -1000.  /         !! min. of Rif
      DATA       RIFMAX /    0.2   /         !! max. of Rif
      DATA       SHTMIN /     0.   /         !! min. of tilde(Sh)
      DATA       SMTMIN /     0.   /         !! min. of tilde(Sm)
      DATA       DFMMIN /    0.15  /         !! min. diff. coef. of u
      DATA       DFMMAX / 10000.   /         !! max. diff. coef. of u
      DATA       FZDF   /    2.D0  /         !! z-dep. increment of min.
      DATA       ZDF0   /    3.D3  /         !! z-dep. center
      DATA       ZDFE   /    1.D3  /         !! z-dep. width
      DATA       A1    , B1    , A2    , B2    , C1
     &         / 0.92  , 16.6  , 0.74  , 10.1  , 0.08   /
      DATA       OFIRST / .TRUE. /
*
*          < 1. constant >
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ VDFY: VERTICAL DIFF. YM82 94/01/19'
         OFIRST = .FALSE.
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMVDFY, END=1190 )
 1190    WRITE  ( JFPAR, NMVDFY )
*
         GAM1 = 1./3. - 2.*A1/B1
         GAM2 = B2/B1 + 6.*A1/B1
         ALP1 = 3.* A2 * GAM1
         ALP2 = 3.* A2 * ( GAM1 + GAM2 )
         BET1 = A1 * B1 * ( GAM1 - C1 )
         BET2 = A1 * ( B1 * ( GAM1 - C1 ) + 6.*A1 + 3.*A2 )
         BET3 = A2 * B1 * GAM1
         BET4 = A2 * ( B1 * ( GAM1 + GAM2 ) - 3.*A1 )
         FAC1 = BET1 / ( 2.* BET2 )
         FAC2 = BET4 / ( 2.* BET2 )
         FAC3 = 4.* BET2 * BET3 / ( 2.*BET2 )**2
         RIFC = GAM1 / ( GAM1 + GAM2 )
      ENDIF
*
*          < 2. flux Ri number >
*
      DO 2100 K = 1, KMAX
         DO 2110 IJ = 1, IJSDIM
            RIF ( IJ,K ) = FAC1 + FAC2 * RIB( IJ,K )
     &                   - SQRT(  ( FAC1 + FAC2 * RIB( IJ,K ) )**2
     &                            - FAC3 * RIB( IJ,K )              )
            RIF ( IJ,K ) = MAX( MIN( RIF( IJ,K ),RIFMAX ), RIFMIN )
 2110    CONTINUE
 2100 CONTINUE
*
*          < 3. tilde(Sm), tilde(Sh) >
*
      DO 3100 K = 1, KMAX
         DO 3110 IJ = 1, IJSDIM
            IF ( RIF ( IJ,K ) .LT. RIFC ) THEN
               SHT ( IJ,K ) =  ( ALP1 - ALP2 * RIF( IJ,K ) )
     &                        /( 1.   -        RIF( IJ,K ) )
               SMT ( IJ,K ) =  ( BET1 - BET2 * RIF( IJ,K ) )
     &                        /( BET3 - BET4 * RIF( IJ,K ) )
     &                        * SHT ( IJ,K )
               SHT ( IJ,K ) = MAX( SHT( IJ,K ), SHTMIN )
               SMT ( IJ,K ) = MAX( SMT( IJ,K ), SMTMIN )
            ELSE
               SHT ( IJ,K ) = SHTMIN
               SMT ( IJ,K ) = SMTMIN
            ENDIF
 3110    CONTINUE
 3100 CONTINUE
*
*          < 4. mixing length >
*
      DO 4100 K = 1, KMAX
         DO 4110 IJ = 1, IJSDIM
            FRZ         = FKARM*( GDZM( IJ,K ) - GDZM( IJ,1 ) )
            AML( IJ,K ) = FRZ / ( 1.D0 + FRZ/AML0 )
 4110    CONTINUE 
 4100 CONTINUE 
*
*          < 5. diffusion coef. >
*
      DO 5100 K = 1, KMAX
         DO 5110 IJ = 1, IJSDIM
            DFX  = AML ( IJ,K ) **2 * DVDZ( IJ,K )
     &             * SQRT( B1*( 1.D0 - RIF( IJ,K ) )*SMT( IJ,K ) )
            DFM( IJ,K ) = DFX * SMT( IJ,K )
 5110    CONTINUE
 5100 CONTINUE
*
      DO 5200 K = 1, KMAX
         DO 5210 IJ = 1, IJSDIM
            ZF   = ( GDZM( IJ,K ) -GDZM( IJ,1 ) - ZDF0 )/ZDFE
            ZFCT = 1.D0 + FZDF *( 1.D0 - TANH( ZF ) ) * 0.5D0 
            DFM( IJ,K ) = MAX( MIN( DFM( IJ,K ),DFMMAX ), ZFCT*DFMMIN )
 5210    CONTINUE
 5200 CONTINUE
*
      DO 5300 IJ = 1, IJSDIM
         DFM( IJ,1 ) = 0.  
 5300 CONTINUE
*
      RETURN
      END
