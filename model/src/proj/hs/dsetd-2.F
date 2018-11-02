* PACKAGE DSETD  !! dynamics: set constants (horizontal diffusion)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/06/15(numaguti) LMAX=0(x-z) model Laplacian
*         00/04/17(hiro)     Nigam&Linzen's U-depend Rayleigh friction
*         00/05/17(hiro)     DSETBTD
*         00/05/18(hiro)     DSETVDF, implicit vertical diffusion
*         01/06/15(hiro)     v.diffusion for moisture
*         05/05/24(hiro)     for initializing BS
***********************************************************************
      SUBROUTINE DSETBTD        !! coefficient for boundary T-diss.
     O         ( SDTOP , SDBTM , TDMP ,
     I           GAT     )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     SDTOP ( IJDIM ) !! coefficient at top
      REAL*8     SDBTM ( IJDIM ) !! coefficient at bottom
      REAL*8     TDMP            !! damping coefficient for temperature
*
*   [INPUT] 
      REAL*8     GAT  ( IJDIM, KMAX ) !! basic state temperature
*
*   [INTERNAL WORK] 
      REAL*8     TMT1, TMT2, STT1, STT2
      REAL*8     TMB1, TMB2, STB1, STB2
      INTEGER    IJ
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL PARAM] 
      REAL*8     TDMPC          !! damping coefficient for temperature
      NAMELIST  /NMBTDIF/ TDMPC
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
      DATA       TDMPC  / 1.D-3  /

      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSETBTD: SET BOUNDARY DIFFUSION 00/05/17'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMBTDIF, END=190 )
  190    WRITE  ( JFPAR, NMBTDIF )
         TDMP  = MAX( TDMPC, 0.D0 )
      ENDIF
*
      DO 1000 IJ = 1, IJDIM
         TMT1 = ( GAT( IJ, KMAX ) + GAT( IJ, KMAX-1 ) ) / 2.D0
         TMT2 = ( GAT( IJ, KMAX-1 ) + GAT( IJ, KMAX-2 ) ) / 2.D0
         STT1 = ( TMT1 - GAT( IJ, KMAX ) ) / DSIG( KMAX )
         STT2 = ( TMT2 - TMT1             ) / DSIG( KMAX-1 )

         TMB1 = ( GAT( IJ, 2    ) + GAT( IJ, 1      ) ) / 2.D0
         TMB2 = ( GAT( IJ, 3    ) + GAT( IJ, 2      ) ) / 2.D0
         STB1 = ( GAT( IJ, 1    ) - TMB1 ) / DSIG( 1 )
         STB2 = ( TMB1            - TMB2 ) / DSIG( 2 )

         SDTOP( IJ ) = TDMP * STT1 / STT2 
         SDBTM( IJ ) = TDMP * STB1 / STB2 
 1000 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE DSETVDF         !!  implicit vertical diffusion coef.
*
*   [PARAMETER]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zcord.F"                /* coordinate             */
*
*   [INTERNAL WORK]
      INTEGER    KJ
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL SAVE]
      REAL*8     A( KMAX )
      REAL*8     B( KMAX )
      REAL*8     C( KMAX )
      REAL*8     VDFV( KMAX )
      REAL*8     VDFD( KMAX )
      REAL*8     VDFT( KMAX )
      REAL*8     VDFQ( KMAX )
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      LOGICAL    ODIFF( NMDIM )
      SAVE       A, B, C
      SAVE       VDFV, VDFD, VDFT
      SAVE       NMO
      SAVE       ODIFF
*
*   [ENTRY MODIFY] 
      REAL*8     WDX ( NMDIM , KMAX     )  !! vor/div/tmp
      REAL*8     WTX ( NMDIM , KMAX     )  !! dt vor/div/tmp
*
*   [ENTRY INPUT]
      REAL*8     DELT
      CHARACTER  CHVAR*10
*
*   [ENTRY WORK]
      REAL*8     U ( NMDIM , KMAX     )  
      REAL*8     V ( NMDIM , KMAX     )  
      REAL*8     VDF( KMAX )
      REAL*8     A1, B1, C1, WV
      INTEGER    K, NM
*
*   [INTERNAL PARAM] 
      REAL*8     S2D            !! second in 1 day
      REAL*8     VDIFV(KMAX)    !! vertical diffusion coeff. (days)
      REAL*8     VDIFD(KMAX)    !! vertical diffusion coeff. (days)
      REAL*8     VDIFT(KMAX)    !! vertical diffusion coeff. (days)
      REAL*8     VDIFQ(KMAX)    !! vertical diffusion coeff. (days)

      LOGICAL    OFIRST
      LOGICAL    ODFV, ODFD, ODFT, ODFQ

      NAMELIST  /NMVDIF/ VDIFV, VDIFD, VDIFT, VDIFQ

      DATA       S2D    / 86400.D0 /
      DATA       VDIFV  / KMAX*0.0D0 /
      DATA       VDIFD  / KMAX*0.0D0 /
      DATA       VDIFT  / KMAX*0.0D0 /
      DATA       VDIFQ  / KMAX*0.0D0 /
      DATA       OFIRST / .TRUE. /
      DATA       ODFV   / .TRUE. /   
      DATA       ODFD   / .TRUE. /   
      DATA       ODFT   / .TRUE. /   
      DATA       ODFQ   / .TRUE. /   

      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSETVDF: SET VERTICAL DIFFUSION 01/05/16'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMVDIF, END=190 )
  190    WRITE  ( JFPAR, NMVDIF )
         DO 100 K = 1, KMAX
            IF( VDIFV( K ) .LE. 0.D0 ) THEN
               IF( ODFV ) WRITE ( 6,* )
     $              '    ### VORTICITY: NOT APPLIED ###' 
               ODFV = .FALSE.
            ELSE
               VDFV( K ) = 1.D0 / ( S2D * VDIFV( K ) )
            ENDIF
            IF( VDIFD( K ) .LE. 0.D0 ) THEN
               IF( ODFD ) WRITE ( 6,* )
     $              '    ### DIVERGENCE: NOT APPLIED ###' 
               ODFD = .FALSE.
            ELSE
               VDFD( K ) = 1.D0 / ( S2D * VDIFD( K ) )
            ENDIF
            IF( VDIFT( K ) .LE. 0.D0 ) THEN
               IF( ODFT ) WRITE ( 6,* )
     $              '    ### TEMPERATURE: NOT APPLIED ###' 
               ODFT = .FALSE.
            ELSE
               VDFT( K ) = 1.D0 / ( S2D * VDIFT( K ) )
            ENDIF
            IF( VDIFQ( K ) .LE. 0.D0 ) THEN
               IF( ODFQ ) WRITE ( 6,* )
     $              '    ### HUMIDITY: NOT APPLIED ###' 
               ODFQ = .FALSE.
            ELSE
               VDFQ( K ) = 1.D0 / ( S2D * VDIFQ( K ) )
            ENDIF
  100    CONTINUE
         CALL DSETNM
     O         ( NMO   )
*
*     waves without diffusion
*
         DO 400 NM = 1, NMDIM
            ODIFF( NM ) = .TRUE. 
  400    CONTINUE
         ODIFF( NMO( 1,0,0 ) ) = .FALSE.
         ODIFF( NMO( 2,0,0 ) ) = .FALSE.
         IF ( LMAX. GE. 1 ) THEN
            ODIFF( NMO( 1,0,MIN(1,LMAX) ) ) = .FALSE.  
            ODIFF( NMO( 2,0,MIN(1,LMAX) ) ) = .FALSE.  
         ENDIF
         IF ( ( MMAX. GE. 1 ).AND.( MINT .EQ. 1 ) ) THEN
            ODIFF( NMO( 1,MIN(1,MMAX),0 ) ) = .FALSE.
            ODIFF( NMO( 2,MIN(1,MMAX),0 ) ) = .FALSE.
         ENDIF
      ENDIF
*
      KJ = KMAX
      B( KJ ) = - 1.D0*( DSIGM(KJ) + DSIGM(KJ+1) ) / 
     $        ( DSIG(KJ)*DSIGM(KJ)*DSIGM(KJ+1) )
      A( KJ ) = ( DSIGM(KJ) + DSIGM(KJ+1) ) / 
     $        ( DSIG(KJ)*DSIGM(KJ)*DSIGM(KJ+1) )
      C( KJ ) = 0.D0
*
      KJ = 1
      B( KJ ) = - 1.D0*( DSIGM(KJ) + DSIGM(KJ+1) ) / 
     $        ( DSIG(KJ)*DSIGM(KJ)*DSIGM(KJ+1) )
      C( KJ ) = ( DSIGM(KJ) + DSIGM(KJ+1) ) / 
     $        ( DSIG(KJ)*DSIGM(KJ)*DSIGM(KJ+1) )
      A( KJ ) = 0.D0
*
      DO 500 KJ = KMAX-1, 2, -1
         C( KJ ) = 1.D0 / ( DSIG( KJ ) * DSIGM( KJ+1 ) )
         B( KJ ) = - 1.D0*( DSIGM(KJ) + DSIGM(KJ+1) ) / 
     $        ( DSIG(KJ)*DSIGM(KJ)*DSIGM(KJ+1) )
         A( KJ ) = 1.D0 / ( DSIG( KJ ) * DSIGM( KJ ) )
  500 CONTINUE
*
*
      RETURN
*=========================================================
      ENTRY SOLVDF
     M     ( WDX ,
     M       WTX ,
     I       DELT  , CHVAR  )
*
      IF( CHVAR(1:3) .EQ. 'VOR' ) THEN 
         IF( .NOT. ODFV ) RETURN
         DO 600 K = 1, KMAX
            VDF( K ) = VDFV( K )
  600    CONTINUE
      ELSEIF( CHVAR(1:3) .EQ. 'DIV' ) THEN 
         IF( .NOT. ODFD ) RETURN
         DO 700 K = 1, KMAX
            VDF( K ) = VDFD( K )
  700    CONTINUE
      ELSEIF( CHVAR(1:3) .EQ. 'TMP' ) THEN 
         IF( .NOT. ODFT ) RETURN
         DO 800 K = 1, KMAX
            VDF( K ) = VDFT( K )
  800    CONTINUE
      ELSEIF( CHVAR(1:3) .EQ. 'SPH' ) THEN 
         IF( .NOT. ODFQ ) RETURN
         DO 900 K = 1, KMAX
            VDF( K ) = VDFQ( K )
  900    CONTINUE
      ELSE
         WRITE( JFPAR, *) ' ###### VARIABLE NOT ASSIGNED ###### '
         CALL XABORT( 1 )
      ENDIF

      K = 1
      DO 1000 NM = 1, NMDIM
         B1 = 1.D0 - VDF( K ) * B( K ) * DELT
         C1 = - VDF( K ) * C( K ) * DELT
         U( NM, K ) = - C1 / B1
         V( NM, K ) = WDX( NM, K ) / B1
 1000 CONTINUE
      DO 1100 K = 2, KMAX
         DO 1100 NM = 1, NMDIM
            A1 = - VDF( K ) * A( K ) * DELT
            B1 = 1.D0 - VDF( K ) * B( K ) * DELT
            C1 = - VDF( K ) * C( K ) * DELT
            U( NM, K ) = - C1 / ( A1 * U( NM, K-1 ) + B1 ) 
            V( NM, K ) = ( WDX( NM, K ) - A1 * V( NM, K-1 ) )
     &           / ( A1 * U( NM, K-1 ) + B1 )
 1100 CONTINUE
      K = KMAX
      DO 1200 NM = 1, NMDIM
         IF( CHVAR(1:3) .EQ. 'VOR' .OR. CHVAR(1:3) .EQ. 'DIV' ) THEN
            IF( .NOT. ODIFF(NM) ) GOTO 1200
         ENDIF
         WV = WDX( NM, K )
         WDX( NM, K ) = V( NM, K )
         WTX( NM, K ) = WTX( NM, K ) + ( WDX( NM, K ) - WV ) / DELT
 1200 CONTINUE
      DO 1300 K = KMAX-1, 1, -1
         DO 1300 NM = 1, NMDIM
            IF( CHVAR(1:3) .EQ. 'VOR' .OR. CHVAR(1:3) .EQ. 'DIV' ) THEN
               IF( .NOT. ODIFF(NM) ) GOTO 1300
            ENDIF
            WV = WDX( NM, K ) 
            WDX( NM, K ) = 
     &           U( NM, K ) * WDX( NM, K+1 ) + V( NM, K )
            WTX( NM, K ) = WTX( NM, K ) + ( WDX( NM, K ) - WV ) / DELT
 1300 CONTINUE
*
      RETURN
      END
