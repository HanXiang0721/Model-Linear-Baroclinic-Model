* PACKAGE DSETD  !! dynamics: set constants (horizontal diffusion)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/06/15(numaguti) LMAX=0(x-z) model Laplacian
***********************************************************************
      SUBROUTINE DSETDF    !! horizontal diffusion coefficient
     O         ( DIFV  , DIFD  , DIFT  , DIFQ   )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     DIFV  ( NMDIM, KMAX )       !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX )       !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX )       !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX )       !! h-diff. coef. of water
*
*   [INTERNAL WORK] 
      REAL * 8   EFLSEC
      REAL*8     ZZ, RAYXV, RAYXD
      INTEGER    IDORD
      REAL*8     DIFMAX
      REAL*8     DIFMV, DIFMD, DIFMT, DIFMQ
      INTEGER    L, M, N, NM, LEND, K
      REAL*8     DIFTN, DIFVN
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL SAVE] 
      REAL * 8   EFLSCV
      REAL * 8   EFLSCD
      REAL * 8   EFLSCT
      REAL * 8   EFLSCQ
      INTEGER    IDORDR ( KMAX )
      REAL * 8   EFLSVK ( KMAX )
      REAL * 8   EFLSDK ( KMAX )
      REAL * 8   EFLSTK ( KMAX )
      REAL * 8   EFLSQK ( KMAX )
      REAL * 8   RAYSVK ( KMAX )
      REAL * 8   RAYSDK ( KMAX )
      SAVE       EFLSCV, EFLSCD, EFLSCT, EFLSCQ, IDORDR, 
     &           EFLSVK, EFLSDK, EFLSTK, EFLSQK, RAYSVK, RAYSDK
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      SAVE       NMO
*
*   [INTERNAL PARM] 
      INTEGER    ORDER                       !! order of horiz. diff.
      REAL*8     TEFOLD                      !! damp. t of max. wave #
      REAL*8     TEFV                        !! damp. t of max. wave # V
      REAL*8     TEFD                        !! damp. t of max. wave # V
      REAL*8     TEFT                        !! damp. t of max. wave # T
      REAL*8     TEFQ                        !! damp. t of max. wave # q
      CHARACTER  TUNIT*4                     !! unit of time
      INTEGER    ORDERK( KMAX )              !! order of horiz. diff.
      REAL*8     TEFOLK( KMAX )              !! damp. t of max. wave #
      REAL*8     TEFVK ( KMAX )              !! damp. t of max. wave #
      REAL*8     TEFDK ( KMAX )              !! damp. t of max. wave #
      REAL*8     TEFTK ( KMAX )              !! damp. t of max. wave #
      REAL*8     TEFQK ( KMAX )              !! damp. t of max. wave #
      REAL*8     RAYLEI( KMAX )              !! Rayleigh friction
      REAL*8     RAYLED( KMAX )              !! Rayleigh fric. (div.)
      REAL*8     RAY0                        !! Rayleigh fric.coef.
      REAL*8     RAY0D                       !! Rayleigh fric.coef.div.
      REAL*8     RZOFS                       !! Rayleigh fric.coef.
      REAL*8     RZSCLH                      !! Rayleigh fric.coef.
      REAL*8     ZSCLH                       !! Rayleigh fric.coef.
      REAL*8     DSEC
      LOGICAL    OFIRST
      NAMELIST  /NMHDIF/ ORDER , TEFOLD, TUNIT , 
     &                   TEFV  , TEFD  , TEFT  , TEFQ  , 
     &                   ORDERK, TEFOLK,  
     &                   TEFVK , TEFDK , TEFTK , TEFQK ,
     &                   RAYLEI, RAYLED,
     &                   RAY0  , RAY0D , RZOFS , RZSCLH, ZSCLH
      DATA       ORDER  / 8   /
      DATA       TEFOLD / 1.  /
      DATA       TEFV   / -1. /
      DATA       TEFD   / -1. /
      DATA       TEFT   / -1. /
      DATA       TEFQ   / -1. /
      DATA       TUNIT  / 'DAY' /
      DATA       ORDERK / KMAX*-1 /
      DATA       TEFOLK / KMAX*-1. /
      DATA       TEFVK  / KMAX*-1. /
      DATA       TEFDK  / KMAX*-1. /
      DATA       TEFTK  / KMAX*-1. /
      DATA       TEFQK  / KMAX*-1. /
      DATA       RAYLEI / KMAX*-1. /
      DATA       RAYLED / KMAX*-1. /
      DATA       RAY0   /  30.D0  /
      DATA       RAY0D  /  -1     /
      DATA       RZOFS  / -1.    /
      DATA       RZSCLH / 7.D+3  /
      DATA       ZSCLH  / 8.D+3  /
      DATA       DSEC   / 0.    /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSETC: SET HORIZONTAL DIFFUSION 94/11/30'
         OFIRST = .FALSE.
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMHDIF, END=290 )
  290    WRITE  ( JFPAR, NMHDIF )
*
         IF ( RZOFS .LT. 0. ) THEN
            RZOFS = -ZSCLH*LOG(SIG(KMAX))
         ENDIF
         IF ( RAY0D .LT. 0. ) THEN
            RAY0D = RAY0
         ENDIF
         DO 100 K = 1, KMAX
            ZZ   = -ZSCLH*LOG(SIG(K))
            RAYXV = RAY0 /MAX(1.+TANH((ZZ-RZOFS)/RZSCLH),1.D-20)
            RAYXD = RAY0D/MAX(1.+TANH((ZZ-RZOFS)/RZSCLH),1.D-20)
            CALL CXX2SS ( RAYSVK( K ), RAYXV, TUNIT, DSEC  )
            CALL CXX2SS ( RAYSDK( K ), RAYXD, TUNIT, DSEC  )
  100    CONTINUE 
*
         CALL CXX2SS ( EFLSEC, TEFOLD, TUNIT, DSEC  )
         EFLSCV = EFLSEC
         EFLSCD = EFLSEC
         EFLSCT = EFLSEC
         EFLSCQ = EFLSEC
         IF ( TEFV .GT. 0. ) CALL CXX2SS ( EFLSCV, TEFV, TUNIT, DSEC  )
         IF ( TEFD .GT. 0. ) CALL CXX2SS ( EFLSCD, TEFD, TUNIT, DSEC  )
         IF ( TEFT .GT. 0. ) CALL CXX2SS ( EFLSCT, TEFT, TUNIT, DSEC  )
         IF ( TEFQ .GT. 0. ) CALL CXX2SS ( EFLSCQ, TEFQ, TUNIT, DSEC  )
*
         DO 300 K = 1, KMAX
            IDORDR( K ) = ORDER
            EFLSVK( K ) = EFLSCV
            EFLSDK( K ) = EFLSCD
            EFLSTK( K ) = EFLSCT
            EFLSQK( K ) = EFLSCQ
  300    CONTINUE 
         DO 400 K = 1, KMAX
            IF ( ORDERK( K ) .GT. 0 ) THEN
               IDORDR( K ) = ORDERK( K )
            ENDIF
            IF ( TEFOLK( K ) .GT. 0.) THEN
               CALL CXX2SS ( EFLSVK( K ), TEFOLK( K ), TUNIT, DSEC  )
               EFLSDK( K ) = EFLSVK( K )
               EFLSTK( K ) = EFLSVK( K )
               EFLSQK( K ) = EFLSVK( K )
            ENDIF
            IF ( TEFVK ( K ) .GT. 0. ) THEN
               CALL CXX2SS ( EFLSVK( K ), TEFVK ( K ), TUNIT, DSEC  )
            ENDIF
            IF ( TEFDK ( K ) .GT. 0. ) THEN
               CALL CXX2SS ( EFLSDK( K ), TEFDK ( K ), TUNIT, DSEC  )
            ENDIF
            IF ( TEFTK ( K ) .GT. 0. ) THEN
               CALL CXX2SS ( EFLSTK( K ), TEFTK ( K ), TUNIT, DSEC  )
            ENDIF
            IF ( TEFQK ( K ) .GT. 0. ) THEN
               CALL CXX2SS ( EFLSQK( K ), TEFQK ( K ), TUNIT, DSEC  )
            ENDIF
            IF ( RAYLEI( K ) .GT. 0. ) THEN
               CALL CXX2SS ( RAYSVK( K ), RAYLEI( K ), TUNIT, DSEC  )
               RAYSDK( K ) = RAYSVK( K )
            ENDIF
            IF ( RAYLED( K ) .GT. 0. ) THEN
               CALL CXX2SS ( RAYSDK( K ), RAYLED( K ), TUNIT, DSEC  )
            ENDIF
  400    CONTINUE 
*
         CALL DSETNM
     O         ( NMO   )
      ENDIF
*
      DO 1000 K = 1, KMAX
         IDORD  = IDORDR(K)
         DIFMV  = 1./EFLSVK(K)
         DIFMD  = 1./EFLSDK(K)
         DIFMT  = 1./EFLSTK(K)
         DIFMQ  = 1./EFLSQK(K)
*
         DIFMAX = ( DBLE(NMAX*(NMAX+1))/DBLE(NMAX) )**(IDORD/2)
*
CDIR$ NOVECTOR
         DO 1100 M = 0 , MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
#ifdef SYS_HITAC
*VOPTION NOVEC
#endif
            DO 1110 L = 0 , LEND
               N = L + M
               IF ( N .GT. 0 ) THEN
                  DIFTN  = ( DBLE(N*(N+1))/DBLE(NMAX) )**(IDORD/2)
                  DIFVN  = DIFTN - ( 2.D0/DBLE(NMAX) )**(IDORD/2)
                  DIFV( NMO(1,M,L),K )  = DIFMV * DIFVN / DIFMAX
                  DIFV( NMO(2,M,L),K )  = DIFMV * DIFVN / DIFMAX
                  DIFD( NMO(1,M,L),K )  = DIFMD * DIFVN / DIFMAX
                  DIFD( NMO(2,M,L),K )  = DIFMD * DIFVN / DIFMAX
                  DIFT( NMO(1,M,L),K )  = DIFMT * DIFTN / DIFMAX
                  DIFT( NMO(2,M,L),K )  = DIFMT * DIFTN / DIFMAX
                  DIFQ( NMO(1,M,L),K )  = DIFMQ * DIFTN / DIFMAX
                  DIFQ( NMO(2,M,L),K )  = DIFMQ * DIFTN / DIFMAX
               ENDIF
 1110       CONTINUE
 1100    CONTINUE
 1000 CONTINUE 
CDIR$ VECTOR
*
      DO 2100 K = 1, KMAX
         DIFV ( NMO( 1,0,0 ),K ) = 0.  
         DIFV ( NMO( 2,0,0 ),K ) = 0.  
         DIFD ( NMO( 1,0,0 ),K ) = 0.  
         DIFD ( NMO( 2,0,0 ),K ) = 0.  
         DIFT ( NMO( 1,0,0 ),K ) = 0.  
         DIFT ( NMO( 2,0,0 ),K ) = 0.  
         DIFQ ( NMO( 1,0,0 ),K ) = 0.  
         DIFQ ( NMO( 2,0,0 ),K ) = 0.  
         IF ( LMAX. GE. 1 ) THEN
            DIFV ( NMO( 1,0,MIN(1,LMAX) ),K ) = 0.  
            DIFV ( NMO( 2,0,MIN(1,LMAX) ),K ) = 0.  
            DIFD ( NMO( 1,0,MIN(1,LMAX) ),K ) = 0.  
            DIFD ( NMO( 2,0,MIN(1,LMAX) ),K ) = 0.  
         ENDIF
         IF ( ( MMAX. GE. 1 ).AND.( MINT .EQ. 1 ) ) THEN
            DIFV ( NMO( 1,MIN(1,MMAX),0 ),K ) = 0.  
            DIFV ( NMO( 2,MIN(1,MMAX),0 ),K ) = 0.  
            DIFD ( NMO( 1,MIN(1,MMAX),0 ),K ) = 0.  
            DIFD ( NMO( 2,MIN(1,MMAX),0 ),K ) = 0.  
         ENDIF
 2100 CONTINUE 
*
*        Rayleigh Friction
*
      DO 3100 K = 1, KMAX
         IF ( RAYSVK( K ) .GT. 0. ) THEN
            DO 3110 NM = 1, NMDIM
               DIFV( NM,K ) = DIFV( NM,K ) + 1./RAYSVK( K )
 3110       CONTINUE 
         ENDIF
         IF ( RAYSDK( K ) .GT. 0. ) THEN
            DO 3120 NM = 1, NMDIM
               DIFD( NM,K ) = DIFD( NM,K ) + 1./RAYSDK( K )
 3120       CONTINUE 
         ENDIF
 3100 CONTINUE 
*
      RETURN
      END
