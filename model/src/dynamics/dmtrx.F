* PACKAGE DMTRX !! dynamics: matrix for time integration
*
*   [HIS] 95/07/27(numaguti) FLAPLA
*         96/05/28(numaguti) restructure
**********************************************************************
      SUBROUTINE DMTRIX    !! LU matrix for time integration
     O         ( DMLU  , KPIV  ,
     I           DELT  ,
     C           WHMTX , GCMTX ,
     C           DIFD  , DIFT  , FLAPLA  )
*
*               implicit diffusion
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     DMLU  ( NMDIM, KMAX, KMAX ) !! LU decomp. of SI matrix
      INTEGER    KPIV  ( NMDIM, KMAX       ) !! pivot of SI matrix
*
*   [INPUT] 
      REAL*8     DELT                        !! time step delta(t)
      REAL*8     WHMTX ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     GCMTX ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     DIFD  ( NMDIM, KMAX )       !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX )       !! h-diff. coef. of heat
      REAL*8     FLAPLA( NMDIM )             !! Laplacian factor
*
*   [INTERNAL WORK] 
      REAL*8     DM    ( KMAX, KMAX )        !! work
      INTEGER    KP    ( KMAX )              !! pivot
*
      INTEGER    MXNM, L, M, N, NM, K, KK
*
*   [INTERNAL SAVE]       
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      SAVE       NMO
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DMTRX: SEMI-IMP. MATRIX 95/07/27'
         OFIRST = .FALSE.
         CALL    DSETNM
     O         ( NMO     )
      ENDIF
*
      MXNM = 0
*
      DO 3300 N = 0, NMAX
*                                                  VV=== LOOP N ===
*
*          < 1. semi-implicit matrix >
*
         DO 1000 M = 0, MIN(N,MMAX), MINT
            IF ( N-M .LE. LMAX ) THEN
               NM = NMO( 1, M, N-M )
               GOTO 1010
            ENDIF
 1000    CONTINUE
*                    no n(m,l) ----->  continue
         WRITE (6,*) ' WARNING [DMTRX]: N=', N, ' DOES NOT EXIST '
         GOTO 3300
*
 1010    CONTINUE
*
         DO 1100 K = 1, KMAX
            DO 1100 KK = 1, KMAX
               DM ( K,KK ) = - DELT**2 * FLAPLA( NM )
     &                         * (   WHMTX( K,KK )
     &                             + GCMTX( K,KK )
     &                              *( 1.D0+DELT*2.D0*DIFT(NM,K) ) )
               IF ( K .EQ. KK) THEN
                  DM ( K,KK ) = DM ( K,KK )
     &                        +   ( 1.D0 + DELT*2.D0* DIFD(NM,K) )
     &                          * ( 1.D0 + DELT*2.D0* DIFT(NM,K) )
               ENDIF
 1100    CONTINUE
*
*          < 2. LU matrix >
*
         CALL LUMAKE
     M         ( DM    ,
     O           KP    ,
     D           1     , KMAX   )
*
*          < 3. repack array >
*
         DO 3200 M = 0 , MMAX, MINT
            L = N - M
*
            IF (   ( L .GE. 0 ) .AND. ( L .LE. LMAX )   )THEN
               DO 3100 K = 1, KMAX
*
                  DO 3110 KK = 1, KMAX
                     DMLU ( NMO(1,M,L),K,KK ) = DM ( K,KK )
                     DMLU ( NMO(2,M,L),K,KK ) = DM ( K,KK )
 3110             CONTINUE
                  KPIV ( NMO(1,M,L),K ) = KP ( K )
                  KPIV ( NMO(2,M,L),K ) = KP ( K )
*
                  MXNM = MAX( MXNM, NMO(1,M,L) )
                  MXNM = MAX( MXNM, NMO(2,M,L) )
 3100          CONTINUE
            ENDIF
*
 3200    CONTINUE
*
*                                                  ^^=== LOOP N ===
 3300 CONTINUE
*
      DO 4100 NM = MXNM+1, NMDIM
         DO 4100 K = 1, KMAX
             DO 4110 KK = 1, KMAX
                DMLU ( NM,K,KK ) = DM ( K,KK )
 4110        CONTINUE
             KPIV ( NM,K ) = KP ( K )
 4100 CONTINUE
            
      RETURN
      END
