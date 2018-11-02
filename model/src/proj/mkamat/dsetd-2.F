* PACKAGE DSETD  !! dynamics: set constants (horizontal diffusion)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/06/15(numaguti) LMAX=0(x-z) model Laplacian
*         00/04/17(hiro)     Nigam&Linzen's U-depend Rayleigh friction
*         00/05/17(hiro)     DSETBTD
*         00/05/18(hiro)     DSETVDF, implicit vertical diffusion
*         01/06/15(hiro)     v.diffusion for moisture
***********************************************************************
      SUBROUTINE DSETDF    !! horizontal diffusion coefficient
     O         ( DIFV  , DIFD  , DIFT  , DIFQ  ,
     I           GFU     )
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
*   [INPUT] 
      REAL*8     GFU  ( IJDIM, KMAX ) !! zonal mean wind for the basic state
*
*   [INTERNAL WORK] 
      REAL*8     GU  ( IJDIM, KMAX ) !! zonal mean wind for the basic state
      REAL*8     WR( NMDIM, KMAX ) !! Rayleigh term for critical surfaces
      REAL*8     EFLSEC
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
      REAL*8     RAYCL0                      !! Rayleigh fric.coef.
      REAL*8     RAYCL                       !! Rayleigh fric.coef.
      LOGICAL    ORAYCL                      !! use Nigam&Linzen friction?
      LOGICAL    OFIRST
      NAMELIST  /NMHDIF/ ORDER , TEFOLD, TUNIT , 
     &                   TEFV  , TEFD  , TEFT  , TEFQ  , 
     &                   ORDERK, TEFOLK,  
     &                   TEFVK , TEFDK , TEFTK , TEFQK ,
     &                   ORAYCL, RAYCL0, RAYLEI, RAYLED,
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
      DATA       ORAYCL / .FALSE. /
      DATA       RAYLEI / KMAX*-1. /
      DATA       RAYLED / KMAX*-1. /
      DATA       RAYCL0 /  30.D0  /
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
               DIFV( NM,K) = DIFV( NM,K) + 1./RAYSVK( K ) + WR( NM,K)
 3110       CONTINUE 
         ENDIF
         IF ( RAYSDK( K ) .GT. 0. ) THEN
            DO 3120 NM = 1, NMDIM
               DIFD( NM,K) = DIFD( NM,K) + 1./RAYSDK( K ) + WR( NM,K)
 3120       CONTINUE 
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
c         write(45) ((sngl(gu(i,j,k)),i=1,imax),j=1,jdim)
 1100 CONTINUE
c      close(45)
*
      RETURN
      END
***********************************************************************
      SUBROUTINE DSETBTD        !! coefficient for boundary T-diss.
     O         ( SDTOP , SDBTM , TDMP ,
     I           GFT     )
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
      REAL*8     GFT  ( IJDIM, KMAX ) !! basic state temperature
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
         TMT1 = ( GFT( IJ, KMAX ) + GFT( IJ, KMAX-1 ) ) / 2.D0
         TMT2 = ( GFT( IJ, KMAX-1 ) + GFT( IJ, KMAX-2 ) ) / 2.D0
         STT1 = ( TMT1 - GFT( IJ, KMAX ) ) / DSIG( KMAX )
         STT2 = ( TMT2 - TMT1             ) / DSIG( KMAX-1 )

         TMB1 = ( GFT( IJ, 2    ) + GFT( IJ, 1      ) ) / 2.D0
         TMB2 = ( GFT( IJ, 3    ) + GFT( IJ, 2      ) ) / 2.D0
         STB1 = ( GFT( IJ, 1    ) - TMB1 ) / DSIG( 1 )
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
