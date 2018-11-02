* PACKAGE DADMN !! dynamics: main routine
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/03(numaguti) tracer
*         96/06/06(numaguti) remove FIXMAS/MASFIX
*         01/06/18(hiro)     barotropic model
**********************************************************************
      SUBROUTINE DYNMCS    !!  dynamics
     M         ( GAU   , GAV   ,
     I           GFPSI , GDZS  , 
     I           TIME  , DELT  , OINIT  )
*
*               semi-implicit time integration
*               implicit diffusion
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zhdim.F"                /* # of char. in string   */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [INPUT]
      REAL*8     GFPSI ( IJDIM, KMAX      )  !! BS stream function
      REAL*8     GDZS  ( IJDIM            )  !! topography

      REAL*8     TIME                        !! time
      REAL*8     DELT                        !! time step delta(t)
      LOGICAL    OINIT                       !! initialized condition ?
*
*   [MODIFY]
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind v
*
*   [INTERNAL WORK]
      REAL*8     WTVOR ( NMDIM, KMAX   )  !! tendency of vorticity
      REAL*8     WAVOR ( NMDIM, KMAX   )  !! tendency of vorticity
      REAL*8     WXVOR ( NMDIM, KMAX   )  !! tendency of vorticity
      REAL*8     WFPSI ( NMDIM, KMAX   )  !! tendency of vorticity
      REAL*8     FCT
*
      INTEGER    IFPAR, JFPAR
      INTEGER    NM
      INTEGER    NXVOR
      INTEGER    IW, IS
      INTEGER    IJ, K, LL, MM, LSTR
*
*   [INTERNAL SAVE] 
      REAL*8     GFU   ( IJDIM, KMAX      )  !! BS westerly u 
      REAL*8     GFV   ( IJDIM, KMAX      )  !! BS southern wind v
      REAL*8     DIFV  ( NMDIM, KMAX      )  !! h-diff. coef (vor)
      REAL*8     UVFACT( IJDIM            )  !! factor of u -> U
      REAL*8     WTUNTT( NMDIM, KMAX      )  !! unit matrix
*
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER    NOMWV ( NMDIM )
      INTEGER    M, L, IC, LEND, LEND0
*
      SAVE       GFU, GFV, DIFV, UVFACT
      SAVE       NMO, NOMWV
      SAVE       M, L, LEND, LEND0
*
      REAL*8     CORIOL( IJDIM )          !! Corioris factor f
      SAVE       CORIOL
*     
*   [INTERNAL PARM] 
      CHARACTER  AMATF *(NFILN) !! file name for the matrix column
      INTEGER    KMATW          !! position of adding unity in wave
      INTEGER    KMATL          !! position of adding unity in sigma
      LOGICAL    OSYM           !! equatorial symmetry
      NAMELIST  /NMAMAT/ AMATF, KMATW, KMATL, OSYM
      SAVE       KMATW, KMATL
      LOGICAL    OFIRST
      DATA       AMATF  / '     ' /
      DATA       OFIRST / .TRUE.  /
      LOGICAL    OVWRT
      DATA       OVWRT  / .FALSE. /
      DATA       KMATW  / 0       /
      DATA       KMATL  / 1       /
      DATA       OSYM   / .FALSE. /
*
      REAL*8     UNTV           !! unity for vorticity
      DATA       UNTV   / 1.0D-7 /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADMN: DYNAMICS CONTROL 98/10/30'
         OFIRST = .FALSE.
*
         CALL DSETNM
     O         ( NMO    )
         CALL SPSTUP      !! spherical harmonic functions
         CALL   DSETUV
     O        ( UVFACT )
         DO 100 NM = 1, NMDIM
            NOMWV( NM ) = 1
  100    CONTINUE 
*
*     BS psi --> U V
*
         CALL G2W
     O         ( WFPSI,
     I           GFPSI,
     I           '    ', 'POSO', KMAX )
         CALL W2G
     O         ( GFU   ,
     I           WFPSI ,
     I           'YGRA', 'NEG ', KMAX )
         CALL W2G
     M         ( GFV   ,
     I           WFPSI ,
     I           'XGRA', 'POS ', KMAX )
         DO 200 K = 1, KMAX
            DO 200 IJ = 1, IJDIM
               GFU ( IJ,K ) = GFU ( IJ,K ) / UVFACT( IJ ) / ER
               GFV ( IJ,K ) = GFV ( IJ,K ) / UVFACT( IJ ) / ER
  200    CONTINUE
*
*     h --> h*
*
         CALL G2W
     O         ( WFPSI,
     I           GDZS ,
     I           '    ', 'POSO', 1 )
         DO 210 NM = 1, NMDIM
            DO 210 L = 1, LMAX
               WFPSI ( NMO( 1,0,L ),1 ) = 0.  
               WFPSI ( NMO( 2,0,L ),1 ) = 0.  
  210    CONTINUE
         CALL W2G
     O         ( GDZS  ,
     I           WFPSI ,
     I           '    ', 'POSO', 1 )
*
         CALL DSETDF      !! horizontal diffusion
     O          ( DIFV  , 
     I            GFU     )
*
         CALL DSETCO  
     O         ( CORIOL )
*
#ifdef OPT_MKMAT
*
*     output file for the matrix column
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMAMAT, END=290 )
  290    WRITE  ( JFPAR, NMAMAT )
         CALL FOPNEOF ( 77, AMATF )
         WRITE( 6, *) '    $$$ MATRIX FILE:', AMATF,' $$$'
         IF( OVWRT ) CALL FREWND ( 77 )
*
         M = KMATW
         LEND0 = MIN( LMAX, NMAX-M)
#endif
*
#ifdef OPT_WVFRC
*
*     output file for the matrix column
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMAMAT, END=290 )
  290    WRITE  ( JFPAR, NMAMAT )
         CALL FOPNEOF ( 77, AMATF )
         WRITE( 6, *) '    $$$ MATRIX FILE:', AMATF,' $$$'
         IF( OVWRT ) CALL FREWND ( 77 )
*
         M = KMATW
         LEND0 = MIN( LMAX, NMAX-M)
#endif
*
      ENDIF
*
#ifdef OPT_MKMAT
*
*     loop for l
*
      L = -1
  111 LEND = LEND0
      L = L + 1
      IF( OSYM .AND. MOD( L,2 ) .EQ. 1 ) GOTO 111
      IF( L .GT. LEND ) THEN
         CLOSE( 77 )
         RETURN
      ENDIF
      IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 111 !! global mean
*
*     loop for Re/Im
*
      IC = 0
  222 LEND = LEND0
      IC = IC + 1
      IF( IC .GT. 2 ) GOTO 111
      IF( M .EQ. 0 .AND. IC .EQ. 2 ) GOTO 111 !! imaginary for zonal comp.
*
*     unit perturbation
*
      CALL RESET( WTUNTT, NMDIM*KMAX )
*
      WTUNTT( NMO(IC,M,L), KMATL) = UNTV
#endif
*
*  << tendency term >>
*
      CALL DYNTRM    !! dynamics terms
     O         ( WTVOR , WAVOR ,
     M           GAU   , GAV   , 
     I           GFU   , GFV   , GDZS  , WTUNTT,
     C           CORIOL                  )
*
*  << time integration >>
*
* note 01/06/15 Q used only for LT=1 
      CALL DYNSTP
     M         ( WTVOR , WAVOR ,
     I           DELT  , OINIT , DIFV  , NOMWV   )
*
*     write wave comp. with unity
*
      MM = M
#ifdef OPT_OWALL
      NXVOR = 0
      FCT = -1.D0 / UNTV
      DO 500 MM = 0, MMAX
         LEND = MIN( LMAX, NMAX-MM)
         DO 300 K = 1, KMAX
#else
#ifdef OPT_WVFRC
      NXVOR = 0
      FCT = 1.D0
      DO 500 MM = 0, MIN( M,MMAX )
         LEND = MIN( LMAX, NMAX-MM)
         DO 300 K = 1, KMAX
            NXVOR = 0
#else
         FCT = -1.D0 / UNTV
         DO 300 K = 1, KMAX
            NXVOR = 0
#endif
#endif
            LSTR = 0
            IF( MM .EQ. 0 ) LSTR = 1
            DO 310 LL = LSTR, LEND
               IF( OSYM .AND. MOD( LL,2 ) .EQ. 1 ) GOTO 310
               IF( K .EQ. 1 ) NXVOR = NXVOR + 1
               WXVOR( NXVOR, K) = FCT * WTVOR( NMO(1,MM,LL), K)
               IF( MM .NE. 0 ) THEN
                  NXVOR = NXVOR + 1
                  WXVOR( NXVOR, K) = FCT * WTVOR( NMO(2,MM,LL), K)
               ENDIF
  310       CONTINUE
  300    CONTINUE
#ifdef OPT_WVFRC
      WRITE( 6, *) ' @@@ WRITE DOWN MATRIX FILE : m=',
     &     mm,' l=',l,' rank=',nxvor,' @@@'
         WRITE( 77 ) ((WXVOR(IW,IS),IW=1,NXVOR),IS=1,KMAX)
#endif
  500 CONTINUE
*
#ifndef OPT_WVFRC
      WRITE( 6, *) ' @@@ WRITE DOWN MATRIX FILE : m=',
     &     m,' l=',l,' ic=',ic,' rank=',nxvor,' @@@'
      WRITE( 77 ) ((WXVOR(IW,IS),IW=1,NXVOR),IS=1,KMAX)
#endif
*
#ifdef OPT_MKMAT
      GOTO 222
      GOTO 111
#endif
*
      CLOSE( 77 )
      RETURN
      END
***********************************************************************
      SUBROUTINE DSETDF    !! horizontal diffusion coefficient
     O         ( DIFV  ,
     I           GFU     )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     DIFV  ( NMDIM, KMAX )       !! h-diff. coef (vor)
*
*   [INPUT] 
      REAL*8     GFU  ( IJDIM, KMAX ) !! zonal mean wind for the basic state
*
*   [INTERNAL WORK] 
      REAL*8     GU  ( IJDIM, KMAX ) !! zonal mean wind for the basic state
      REAL*8     WR( NMDIM, KMAX ) !! Rayleigh term for critical surfaces
      REAL*8     EFLSEC
      REAL*8     ZZ, RAYXV
      INTEGER    IDORD
      REAL*8     DIFMAX
      REAL*8     DIFMV
      INTEGER    L, M, N, NM, LEND, K
      REAL*8     DIFTN, DIFVN
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL SAVE] 
      REAL * 8   EFLSCV
      INTEGER    IDORDR ( KMAX )
      REAL * 8   EFLSVK ( KMAX )
      REAL * 8   RAYSVK ( KMAX )
      SAVE       EFLSCV, IDORDR, 
     &           EFLSVK, RAYSVK
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      SAVE       NMO
*
*   [INTERNAL PARM] 
      INTEGER    ORDER                       !! order of horiz. diff.
      REAL*8     TEFOLD                      !! damp. t of max. wave #
      REAL*8     TEFV                        !! damp. t of max. wave # V
      CHARACTER  TUNIT*4                     !! unit of time
      INTEGER    ORDERK( KMAX )              !! order of horiz. diff.
      REAL*8     TEFOLK( KMAX )              !! damp. t of max. wave #
      REAL*8     TEFVK ( KMAX )              !! damp. t of max. wave #
      REAL*8     RAYLEI( KMAX )              !! Rayleigh friction
      REAL*8     RAY0                        !! Rayleigh fric.coef.
      REAL*8     RZOFS                       !! Rayleigh fric.coef.
      REAL*8     RZSCLH                      !! Rayleigh fric.coef.
      REAL*8     ZSCLH                       !! Rayleigh fric.coef.
      REAL*8     DSEC
      REAL*8     RAYCL0                      !! Rayleigh fric.coef.
      REAL*8     RAYCL                       !! Rayleigh fric.coef.
      LOGICAL    ORAYCL                      !! use Nigam&Linzen friction?
      LOGICAL    OFIRST
      LOGICAL    OPRVSB
      NAMELIST  /NMHDIF/ ORDER , TEFOLD, TUNIT , 
     &                   TEFV  , ORDERK, TEFOLK,  
     &                   TEFVK , 
     &                   ORAYCL, RAYCL0, RAYLEI, 
     &                   RAY0  , RZOFS , RZSCLH, ZSCLH
      DATA       ORDER  / 8   /
      DATA       TEFOLD / 1.  /
      DATA       TEFV   / -1. /
      DATA       TUNIT  / 'DAY' /
      DATA       ORDERK / KMAX*-1 /
      DATA       TEFOLK / KMAX*-1. /
      DATA       TEFVK  / KMAX*-1. /
      DATA       ORAYCL / .FALSE. /
      DATA       RAYLEI / KMAX*-1. /
      DATA       RAYCL0 /  30.D0  /
      DATA       RAY0   /  30.D0  /
      DATA       RZOFS  / -1.    /
      DATA       RZSCLH / 7.D+3  /
      DATA       ZSCLH  / 8.D+3  /
      DATA       DSEC   / 0.    /
      DATA       OFIRST / .TRUE. /
      DATA       OPRVSB / .TRUE. / !! preserv solid body rotation 
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSETC: SET HORIZONTAL DIFFUSION 01/06/17'
         OFIRST = .FALSE.
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMHDIF, END=290 )
  290    WRITE  ( JFPAR, NMHDIF )
*
         IF ( RZOFS .LT. 0. ) THEN
            RZOFS = -ZSCLH*LOG(SIG(KMAX))
         ENDIF
         DO 100 K = 1, KMAX
            ZZ   = -ZSCLH*LOG(SIG(K))
            RAYXV = RAY0 /MAX(1.+TANH((ZZ-RZOFS)/RZSCLH),1.D-20)
            CALL CXX2SS ( RAYSVK( K ), RAYXV, TUNIT, DSEC  )
  100    CONTINUE 
*
         CALL CXX2SS ( EFLSEC, TEFOLD, TUNIT, DSEC  )
         EFLSCV = EFLSEC
         IF ( TEFV .GT. 0. ) CALL CXX2SS ( EFLSCV, TEFV, TUNIT, DSEC  )
*
         DO 300 K = 1, KMAX
            IDORDR( K ) = ORDER
            EFLSVK( K ) = EFLSCV
  300    CONTINUE 
         DO 400 K = 1, KMAX
            IF ( ORDERK( K ) .GT. 0 ) THEN
               IDORDR( K ) = ORDERK( K )
            ENDIF
            IF ( TEFOLK( K ) .GT. 0.) THEN
               CALL CXX2SS ( EFLSVK( K ), TEFOLK( K ), TUNIT, DSEC  )
            ENDIF
            IF ( TEFVK ( K ) .GT. 0. ) THEN
               CALL CXX2SS ( EFLSVK( K ), TEFVK ( K ), TUNIT, DSEC  )
            ENDIF
            IF ( RAYLEI( K ) .GT. 0. ) THEN
               CALL CXX2SS ( RAYSVK( K ), RAYLEI( K ), TUNIT, DSEC  )
            ENDIF
  400    CONTINUE 
*
         CALL DSETNM
     O         ( NMO   )
*
         CALL RESET( WR , NMDIM*KMAX )
         IF( ORAYCL ) THEN       !! calculate U-depend Rayleigh coef.
            WRITE ( 6,* ) 
     &           ' @@@ DSETC: NIGAM & LINDZEN RAYLEIGH FRIC. 00/04/18'
            CALL COPY( GU , GFU , IJDIM*KMAX )
            CALL CXX2SS ( RAYCL, RAYCL0, TUNIT, DSEC  )
            CALL RAYLC
     M           ( GU    ,
     I             RAYCL  )
            CALL G2W
     O           (   WR  ,
     I               GU  ,
     F             '    ', 'POS ', KMAX     )
         ENDIF
      ENDIF
*
      DO 1000 K = 1, KMAX
         IDORD  = IDORDR(K)
         DIFMV  = 1./EFLSVK(K)
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
                  IF( OPRVSB ) THEN !! preserve solid body rotation
                     DIFVN  = DIFTN - ( 2.D0/DBLE(NMAX) )**(IDORD/2)
                  ELSE
                     DIFVN = DIFTN
                  ENDIF
                  DIFV( NMO(1,M,L),K )  = DIFMV * DIFVN / DIFMAX
                  DIFV( NMO(2,M,L),K )  = DIFMV * DIFVN / DIFMAX
                  IF( M.GT.0 ) THEN
                     WR( NMO(1,M,L),K )  = WR( NMO(1,M,L),K )
                     WR( NMO(2,M,L),K )  = WR( NMO(2,M,L),K )
                  ENDIF
                  IF( K.LE.4 ) THEN
                     WR( NMO(1,M,L),K )  = 0.D0
                     WR( NMO(2,M,L),K )  = 0.D0
                  ENDIF
               ENDIF
 1110       CONTINUE
 1100    CONTINUE
 1000 CONTINUE 
CDIR$ VECTOR
*
      DO 2100 K = 1, KMAX
         DIFV ( NMO( 1,0,0 ),K ) = 0.  
         DIFV ( NMO( 2,0,0 ),K ) = 0.  
         IF ( LMAX. GE. 1 ) THEN
            DIFV ( NMO( 1,0,MIN(1,LMAX) ),K ) = 0.  
            DIFV ( NMO( 2,0,MIN(1,LMAX) ),K ) = 0.  
         ENDIF
         IF ( ( MMAX. GE. 1 ).AND.( MINT .EQ. 1 ) ) THEN
            DIFV ( NMO( 1,MIN(1,MMAX),0 ),K ) = 0.  
            DIFV ( NMO( 2,MIN(1,MMAX),0 ),K ) = 0.  
         ENDIF
 2100 CONTINUE 
*
*        Rayleigh Friction
*
      DO 3100 K = 1, KMAX
         IF ( RAYSVK( K ) .GT. 0. ) THEN
            DO 3110 NM = 1, NMDIM
               DIFV( NM,K) = DIFV( NM,K) + 1./RAYSVK( K ) + WR( NM,K)
 3110       CONTINUE 
         ENDIF
 3100 CONTINUE 
*
      RETURN
      END
***********************************************************************
      SUBROUTINE RAYLC
     M           ( GU  ,
     I             RAY   )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zcord.F"                /* coordinate             */
*
      INTEGER    MAXNODE
      PARAMETER ( MAXNODE = IJDIM )
*
*     [MODIFY]
      REAL*8     GU( IDIM , JDIM , KMAX )
*
*     [INPUT]
      REAL*8     RAY
*
*     [WORK]
      REAL*8     ALATJ( IDIM , JDIM )
      REAL*8     GUTMP( IDIM , JDIM )
      REAL*8     ALATN( MAXNODE )
      REAL*8     U0, U1, A, B, R, XMISS
      INTEGER    IJ, I, J, K, N, INODE
      DATA       B     / 1.D0 /
      DATA       XMISS / -999.D0 /
*
*
      DO 1000 J = 1, JDIM 
         DO 1000 I = 1, IDIM 
            IJ = (J-1)*IDIM + I
            ALATJ( I, J ) = ALAT( IJ )
 1000 CONTINUE


      DO 1100 K = 1, KMAX
         DO 1110 IJ = 1, MAXNODE
            ALATN( IJ ) = XMISS
 1110    CONTINUE
         DO 1150 I = 1, IDIM
            INODE = 0
            U0 = GU( I, 1, K)
            GUTMP( I, 1) = GU( I, 1, K)
            GU( I, 1, K) = B
            DO 1200 J = 2, JDIM
               U1 = U0 * GU( I, J, K)  
               U0 = GU( I, J, K)
               A = DSIGN( B, U1)
               GUTMP( I, J) = GU( I, J, K)
               GU( I, J, K) = 0.D0
               IF( A .EQ. -B ) THEN
                  INODE = INODE + 1
                  IF( INODE .GT. MAXNODE ) THEN
                     WRITE( 6, *) 
     $                    ' ### TOO MANY CRITICAL SFCs AT:', J,' LEV:',K
                     CALL XABORT( 1 )
                  ENDIF
                  ALATN( INODE ) = ALATJ( I, J )
            ENDIF
 1200    CONTINUE
         GU( I, 1, K) = 0.D0
 1150 CONTINUE
*
         DO 1300 J = 1, JDIM
            DO 1300 I = 1, IDIM
               DO 1400 N = 1, MAXNODE
                  IF( ALATN( N ) .EQ. XMISS ) GOTO 1400
                  A = TANH( 8.D0*(ALATJ(I,J)-ALATN(N) ) )**2
c                  R = ( 1.D0 - (17.D0/18.D0) * A ) / RAY
                  R = ( 1.D0 - (2.D0/3.D0) * A ) / RAY
                  GU( I, J, K) = GU( I, J, K) + MAX( R, 0.D0 )
 1400          CONTINUE
               IF( GUTMP(I,J) .LT. 0.D0 )
     &              GU( I, J, K) = 1.D0 / RAY
 1300    CONTINUE
 1100 CONTINUE
*
      RETURN
      END
