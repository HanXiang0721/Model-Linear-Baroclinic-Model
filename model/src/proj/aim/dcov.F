**********************************************************************
      SUBROUTINE COV    !!  covariance, inv. matrix and forcing
     O         ( WDVOR , WDDIV , WDT   , WDPI  , WDQ   ,
     I           IUNITM, NWTRN , XP                      )
*
*               inverse matrix should be calculated
*               in advance
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
      INTEGER     MAXN
#ifdef OPT_CLASSIC
      PARAMETER ( MAXN = 2*NMAX*(KMAX*3+1) ) !! dry
#else
      PARAMETER ( MAXN = 2*NMAX*(KMAX*4+1) ) !! moist
#endif
*
*   [OUTPUT] 
      REAL*8     WDVOR( NMDIM, KMAX ) !! vorticity
      REAL*8     WDDIV( NMDIM, KMAX ) !! divergence
      REAL*8     WDT  ( NMDIM, KMAX ) !! temperature
      REAL*8     WDPI ( NMDIM       ) !! ln Ps
      REAL*8     WDQ  ( NMDIM, KMAX ) !! humidity
*
*   [INPUT]
      INTEGER    IUNITM          !! unit no. of the inv. matrix
      INTEGER    NWTRN           !! zonal wave truncation
      REAL*8     XP( MAXN,0:MMAX ) !! correction vector -(La-R)
*
*   [INTERNAL]
      REAL*8     X   ( MAXN )
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
      INTEGER    I, J, K, M, L, N, LEND, LDA
      INTEGER    JSTR, JEND
      INTEGER    NM
*
*   [INTERNAL SAVE] 
      REAL*8     A( MAXN*MAXN,0:MMAX )       !! inverse linear matrix
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER    MTR, MVAR

      SAVE       A
      SAVE       MTR, MVAR
      SAVE       NMO
*     
*   [INTERNAL PARM] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE.  /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ COV: SOLVE STEADY RESPONSE 05/05/26'
         OFIRST = .FALSE.
*
         CALL DSETNM
     O         ( NMO    )
*
         MTR  = NWTRN
#ifdef OPT_CLASSIC
         MVAR = 3
#else
         MVAR = 4
#endif
*         
         LEND = 0
         DO 1000 M = 0, NWTRN
            L = NMAX - M + 1
            IF( M .EQ. 0 ) THEN
               L = L - 1
            ELSE
               L = 2 * L
            ENDIF
            LEND = LEND + L
 1000    CONTINUE
*
*     store inverse matrix and external forcing
*
         DO 1100 M = 0, MIN( MTR, NWTRN ) !! zonal wave number
            L = NMAX - M + 1
            IF( M .EQ. 0 ) THEN
               L = L - 1
            ELSE
               L = 2 * L
            ENDIF
            LDA = L * ( MVAR * KMAX + 1 )
            LEND = MIN( LMAX, NMAX-M )
            DO 1200 I = 1, LDA 
               JSTR = (I-1)*LDA + 1
               JEND = JSTR + LDA - 1
               READ( IUNITM ) ( A( J,M ), J = JSTR, JEND )
 1200       CONTINUE
 1100    CONTINUE
         CLOSE( IUNITM )
*
      ENDIF
*
*     covariance
*
      CALL RESET( WDVOR,   NMDIM*KMAX )
      CALL RESET( WDDIV,   NMDIM*KMAX )
      CALL RESET( WDT  ,   NMDIM*KMAX )
      CALL RESET( WDPI ,   NMDIM      )
      CALL RESET( WDQ  ,   NMDIM*KMAX )
      DO 2000 M = 0, MIN( MTR, NWTRN ) !! zonal wave number

         L = NMAX - M + 1
         IF( M .EQ. 0 ) THEN
            L = L - 1
         ELSE
            L = 2 * L
         ENDIF
         LDA = L * ( MVAR * KMAX + 1 )
         LEND = MIN( LMAX, NMAX-M )

         CALL RESET( X,   MAXN            )
         CALL CLCSTR ( '  COVIN' ) 
         DO 2100 I = 1, LDA 
            JSTR = (I-1)*LDA + 1
            JEND = JSTR + LDA - 1
            DO 2200 J = JSTR, JEND
               K = J - JSTR + 1
               X( I ) = X( I ) + A( J,M ) * XP( K,M )
 2200       CONTINUE
 2100    CONTINUE
         CALL CLCEND ( '  COVIN' ) 
*
         NM = 0
         DO 2300 K = 1, KMAX    !! vorticity
            DO 2350 L = 0, LEND  !! meridional wave number
               IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 2350
               NM = NM + 1
               I = NMO( 1,M,L )
               WDVOR( I,K ) = WDVOR( I,K ) + X( NM )
               IF( M .EQ. 0 ) GOTO 2350
               NM = NM + 1
               I = NMO( 2,M,L )
               WDVOR( I,K ) = WDVOR( I,K ) + X( NM )
 2350       CONTINUE
 2300    CONTINUE
*
         DO 2400 K = 1, KMAX    !! divergence
            DO 2450 L = 0, LEND  
               IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 2450
               NM = NM + 1
               I = NMO( 1,M,L )
               WDDIV( I,K ) = WDDIV( I,K ) + X( NM )
               IF( M .EQ. 0 ) GOTO 2450
               NM = NM + 1
               I = NMO( 2,M,L )
               WDDIV( I,K ) = WDDIV( I,K ) + X( NM )
 2450       CONTINUE
 2400    CONTINUE
*
         DO 2500 K = 1, KMAX    !! temperature
            DO 2550 L = 0, LEND  
               IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 2550
               NM = NM + 1
               I = NMO( 1,M,L )
               WDT( I,K ) = WDT( I,K ) + X( NM )
               IF( M .EQ. 0 ) GOTO 2550
               NM = NM + 1
               I = NMO( 2,M,L )
               WDT( I,K ) = WDT( I,K ) + X( NM )
 2550       CONTINUE
 2500    CONTINUE
*
         DO 2650 L = 0, LEND    !! ln Ps
            IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 2650
            NM = NM + 1
            I = NMO( 1,M,L )
            WDPI( I ) = WDPI( I ) + X( NM )
            IF( M .EQ. 0 ) GOTO 2650
            NM = NM + 1
            I = NMO( 2,M,L )
            WDPI( I ) = WDPI( I ) + X( NM )
 2650    CONTINUE
*
#ifndef OPT_CLASSIC
         DO 2700 K = 1, KMAX    !! vorticity
            DO 2750 L = 0, LEND  !! meridional wave number
               IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 2750
               NM = NM + 1
               I = NMO( 1,M,L )
               WDQ( I,K ) = WDQ( I,K ) + X( NM )
               IF( M .EQ. 0 ) GOTO 2750
               NM = NM + 1
               I = NMO( 2,M,L )
               WDQ( I,K ) = WDQ( I,K ) + X( NM )
 2750       CONTINUE
 2700    CONTINUE
#endif

 2000 CONTINUE
*
*
*
      RETURN
      END
