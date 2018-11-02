* PACKAGE PVDFY  !! physics: vurtical diffusion coefficient
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/11/21(numaguti) height-dependent minimum 
***********************************************************************
      SUBROUTINE VDFCOF    !! vertical diffusion coefficient
     O         ( DFM   , DFH   , DFE   ,
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
      REAL*8     DFH   ( IJSDIM, KMAX )   !! trans. coef. :T
      REAL*8     DFE   ( IJSDIM, KMAX )   !! trans. coef. :q
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
      REAL*8     DFMMIN, DFHMIN, DFEMIN
      REAL*8     DFMMAX, DFHMAX, DFEMAX
      REAL*8     FZDF  , ZDF0  , ZDFE  
      REAL*8     A1    , B1    , A2    , B2    , C1
      LOGICAL    OFIRST
      NAMELIST  /NMVDFY/ AML0  ,
     &                   SHTMIN, SMTMIN, 
     &                   RIFMIN, RIFMAX,
     &                   DFMMIN, DFHMIN, DFEMIN,
     &                   DFMMAX, DFHMAX, DFEMAX,
     &                   FZDF  , ZDF0  , ZDFE  ,
     &                   A1    , B1    , A2    , B2    , C1
      DATA       AML0   /   300.   /         !! max. mixing length 
      DATA       RIFMIN /  -1000.  /         !! min. of Rif
      DATA       RIFMAX /    0.2   /         !! max. of Rif
      DATA       SHTMIN /     0.   /         !! min. of tilde(Sh)
      DATA       SMTMIN /     0.   /         !! min. of tilde(Sm)
      DATA       DFMMIN /    0.15  /         !! min. diff. coef. of u
      DATA       DFHMIN /    0.15  /         !! min. diff. coef. of T
      DATA       DFEMIN /    0.15  /         !! min. diff. coef. of q
      DATA       DFMMAX / 10000.   /         !! max. diff. coef. of u
      DATA       DFHMAX / 10000.   /         !! max. diff. coef. of T
      DATA       DFEMAX / 10000.   /         !! max. diff. coef. of q
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
            DFH( IJ,K ) = DFX * SHT( IJ,K )
            DFE( IJ,K ) = DFH( IJ,K )
 5110    CONTINUE
 5100 CONTINUE
*
      DO 5200 K = 1, KMAX
         DO 5210 IJ = 1, IJSDIM
            ZF   = ( GDZM( IJ,K ) -GDZM( IJ,1 ) - ZDF0 )/ZDFE
            ZFCT = 1.D0 + FZDF *( 1.D0 - TANH( ZF ) ) * 0.5D0 
            DFM( IJ,K ) = MAX( MIN( DFM( IJ,K ),DFMMAX ), ZFCT*DFMMIN )
            DFH( IJ,K ) = MAX( MIN( DFH( IJ,K ),DFHMAX ), ZFCT*DFHMIN )
            DFE( IJ,K ) = MAX( MIN( DFE( IJ,K ),DFEMAX ), ZFCT*DFEMIN )
 5210    CONTINUE
 5200 CONTINUE
*
      DO 5300 IJ = 1, IJSDIM
         DFM( IJ,1 ) = 0.  
         DFH( IJ,1 ) = 0.  
         DFE( IJ,1 ) = 0.  
 5300 CONTINUE
*
      RETURN
      END
