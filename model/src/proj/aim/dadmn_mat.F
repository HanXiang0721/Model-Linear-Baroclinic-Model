* PACKAGE DADMN !! dynamics: main routine
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/03(numaguti) tracer
*         96/06/06(numaguti) remove FIXMAS/MASFIX
*         01/06/15(hiro)     PWM incl. moisture
*         05/03/30(hiro)     add additional diffusion R
*         05/05/26(hiro)     AIM for baroclinic model
**********************************************************************
      SUBROUTINE DYNMCS_MAT    !!  dynamics
     I         ( GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           TIME  , DELT  , GDZS  )
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
*   [OUTPUT] 
*
*   [INPUT]
      REAL*8     GFU   ( IJDIM, KMAX      )  !! BS westerly u 
      REAL*8     GFV   ( IJDIM, KMAX      )  !! BS southern wind v
      REAL*8     GFT   ( IJDIM, KMAX      )  !! BS temperature
      REAL*8     GFPS  ( IJDIM            )  !! BS surface pressure
      REAL*8     GFQ   ( IJDIM, KMAX      )  !! BS humidity etc.

      REAL*8     TIME                        !! time
      REAL*8     DELT                        !! time step delta(t)
      REAL*8     GDZS  ( IJDIM )             !! surface topography
*
*   [INTERNAL]
      REAL*8     GAU ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAV ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAT ( IJDIM, KMAX       )  !! perturbation temperature 
      REAL*8     GAPI( IJDIM             )  !! perturbation pi
      REAL*8     GAQ ( IJDIM, KMAX       )  !! perturbation humidity
      REAL*8     GAVOR ( IJDIM, KMAX     )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX     )  !! divergence
*
*   [INTERNAL WORK] 
      REAL*8     WAVOR ( NMDIM, KMAX   )  !! vorticity
      REAL*8     WADIV ( NMDIM, KMAX   )  !! divergence D
      REAL*8     WAT   ( NMDIM, KMAX   )  !! temperature
      REAL*8     WAPI  ( NMDIM         )  !! logPs pi
      REAL*8     WAQ   ( NMDIM, KMAX   )  !! humidity
*
      REAL*8     WTVOR ( NMDIM, KMAX   )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX   )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX   )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM         )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX   )  !! tendency of humidity
*
      REAL*8     WTUNTV( NMDIM, KMAX   )  !! unit matrix
      REAL*8     WTUNTD( NMDIM, KMAX   )  !! unit matrix
      REAL*8     WTUNTT( NMDIM, KMAX   )  !! unit matrix
      REAL*8     WTUNTP( NMDIM         )  !! unit matrix 
      REAL*8     WTUNTQ( NMDIM, KMAX   )  !! unit matrix
*
      REAL*8     WXVOR ( NMDIM, KMAX, 0:MMAX ) !! work
      REAL*8     WXDIV ( NMDIM, KMAX, 0:MMAX ) !! work
      REAL*8     WXTMP ( NMDIM, KMAX, 0:MMAX ) !! work
      REAL*8     WXPI  ( NMDIM      , 0:MMAX ) !! work
      REAL*8     WXSPH ( NMDIM, KMAX, 0:MMAX ) !! work
*
      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work

      INTEGER    IFPAR, JFPAR
      INTEGER    NM
      INTEGER    NX
      INTEGER    NXW( 0:MMAX )
      INTEGER    IW, IS
      INTEGER    L, IC, LSTR, LL, K
      INTEGER    ISTP
*
*   [INTERNAL SAVE] 
      REAL*8     WBVOR ( NMDIM, KMAX   )  !! vorticity
      REAL*8     WBDIV ( NMDIM, KMAX   )  !! divergence D
      REAL*8     WBT   ( NMDIM, KMAX   )  !! temperature
      REAL*8     WBPI  ( NMDIM         )  !! logPs pi
      REAL*8     WBQ   ( NMDIM, KMAX   )  !! humidity
*
      REAL*8     DIFV  ( NMDIM, KMAX   )  !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX   )  !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX   )  !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX   )  !! h-diff. coef. of water 
      REAL*8     DIFP  ( NMDIM         )  !! h-diff. coef (pi)
      REAL*8     WPHIS ( NMDIM  )         !! surface G.P.
*
      REAL*8     SDTOP ( IJDIM         ) !! coefficient for diss.
      REAL*8     SDBTM ( IJDIM         ) !! coefficient for diss.
*
      REAL*8     SALPHA( KMAX )           !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX )           !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX )           !! fact. of vert. intrp. T
      REAL*8     ATF   ( KMAX )           !! fact. of vert. intrp. T
      REAL*8     BTF   ( KMAX )           !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX )           !! basic temp. (full lev)
      REAL*8     GG    ( KMAX )           !! for semi-implicit
      REAL*8     TMTX  ( KMAX, KMAX )     !! for semi-implicit
      REAL*8     WHMTX ( KMAX, KMAX )     !! for semi-implicit
      REAL*8     GCMTX ( KMAX, KMAX )     !! for semi-implicit
      REAL*8     TDMPC          !! damping coefficient for temperature

      REAL*8     CORIOL( IJDIM )          !! Coriolis factor f
      REAL*8     FLAPLA( NMDIM )          !! Laplacian factor

      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER    M, LEND, LEND0

      SAVE       WBVOR, WBDIV, WBT, WBPI, WBQ
      SAVE       DIFV, DIFD, DIFT, DIFQ, DIFP, WPHIS
      SAVE       SDTOP, SDBTM
      SAVE       NMO
*
      SAVE       SALPHA, SBETA, SKAPPA, ATF, BTF, TBAR
      SAVE       GG, TMTX, WHMTX, GCMTX
      SAVE       TDMPC
*
      SAVE       CORIOL, FLAPLA
      SAVE       M, LEND, LEND0
*     
*   [INTERNAL PARM] 
      REAL*8     UNTV           !! unity for vorticity
      REAL*8     UNTD           !! unity for divergence
      REAL*8     UNTT           !! unity for temperature
      REAL*8     UNTP           !! unity for log(Ps)
      REAL*8     UNTQ           !! unity for humidity
      REAL*8     RFACT          !! factor for R used for AIM

      INTEGER    KMATW          !! position of adding unity in wave
      INTEGER    KMATL          !! position of adding unity in sigma
      INTEGER    NSTP

      CHARACTER  AMATF *(NFILN) !! file name for the matrix column

      LOGICAL    OMATV          !! add unity to the vorticity?
      LOGICAL    OMATD          !! add unity to the divergence?
      LOGICAL    OMATT          !! add unity to the temperature?
      LOGICAL    OMATP          !! add unity to the sfc. pressure?
      LOGICAL    OMATQ          !! add unity to the temperature?
      LOGICAL    OFIRST
      LOGICAL    OVWRT
      LOGICAL    OSLNOID        !! include solenoidal term in vor.

      SAVE       OMATV, OMATD, OMATT, OMATP, OMATQ
      SAVE       KMATW, KMATL

      NAMELIST  /NMAMAT/ AMATF, 
     $           OMATV, OMATD, OMATT, OMATP, OMATQ, KMATW, KMATL
      NAMELIST  /NMRDIF/ RFACT
      NAMELIST  /NMSOLE/ OSLNOID
      NAMELIST  /NMNTRM/ NSTP

      DATA       AMATF  / '     ' /
      DATA       OFIRST / .TRUE.  /
      DATA       RFACT  / 0.D0    /
      DATA       OVWRT  / .TRUE.  /
      DATA       OMATV  / .FALSE. /
      DATA       OMATD  / .FALSE. /
      DATA       OMATT  / .FALSE. /
      DATA       OMATP  / .FALSE. /
      DATA       OMATQ  / .FALSE. /
      DATA       OSLNOID / .TRUE. /
      DATA       KMATW  / 0       /
      DATA       KMATL  / 0       /
      DATA       NSTP   / 2       /
*
      DATA       UNTV   / 1.0D-7 /
      DATA       UNTD   / 1.0D-7 /
      DATA       UNTT   / 1.0D-3 /
      DATA       UNTP   / 1.0D-5 /
      DATA       UNTQ   / 1.0D-6 /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADMN: DYNAMICS CONTROL 98/10/30'
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMAMAT, END=290 )
 290     WRITE  ( JFPAR, NMAMAT )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMRDIF, END=390 )
 390     WRITE  ( JFPAR, NMRDIF )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSOLE, END=490 )
 490     WRITE  ( JFPAR, NMSOLE)
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMNTRM, END=590 )
 590     WRITE  ( JFPAR, NMNTRM)
         OFIRST = .FALSE.
*
         IF( RFACT .NE. 0.D0 ) THEN
            CALL CLCSTR ( '  DYNMCS_MAT_R' )
            CALL DYNMCS_MAT_R !!  dynamics, adding R
     I         ( RFACT   )
            CALL CLCEND ( '  DYNMCS_MAT_R' )
            RETURN
         ENDIF

         CALL DSETNM
     O         ( NMO    )
         CALL SPSTUP      !! spherical harmonic functions
         CALL G2W
     O         ( WPHIS ,
     I           GDZS  ,
     F           '    ', 'POS ', 1     )
         DO 1000 NM = 1, NMDIM
***            WPHIS( NM ) = WPHIS( NM )*GRAV
            WPHIS( NM ) = 0.D0
 1000    CONTINUE 
*
         CALL DSETDF      !! horizontal diffusion
     O          ( DIFV  , DIFD  , DIFT  , DIFQ  , DIFP ,
     I            GFU     )
*
         CALL DSETVD      !! vertical difference/semi implicit
     O        ( SALPHA, SBETA , SKAPPA,
     O          ATF   , BTF   , TBAR  , 
     O          GG    , TMTX  , WHMTX , GCMTX ,
     I          GFT    )
*
         CALL DSETCO  
     O         ( CORIOL )
         CALL DSETLP
     O         ( FLAPLA )
         CALL DSETBTD
     O         ( SDTOP , SDBTM , TDMPC ,
     I           GFT     )
         CALL DSETVDF
*
*     output file for the matrix column
*
         IF( AMATF(1:1) .EQ. ' ' ) THEN
            WRITE( 6, *) '    ### NO MATRIX FILE SPECIFIED ### '
            RETURN
         ENDIF
         OPEN( 77, FILE = AMATF, FORM='UNFORMATTED', STATUS='UNKNOWN')
         WRITE( 6, *) '    $$$ MATRIX FILE:', AMATF,' $$$'
         IF( OVWRT ) CALL FREWND ( 77 )
         M = KMATW
         LEND0 = MIN( LMAX, NMAX-M)
*
      ENDIF
*
*     loop for l
*
      L = -1
  111 LEND = LEND0
      L = L + 1
      IF( L .GT. LEND ) THEN
         CLOSE( 77 )
         GOTO 1999
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
      CALL RESET( WAVOR , NMDIM*KMAX )
      CALL RESET( WADIV , NMDIM*KMAX )
      CALL RESET( WAT   , NMDIM*KMAX )
      CALL RESET( WAPI  , NMDIM      )
      CALL RESET( WAQ   , NMDIM*KMAX )

      CALL RESET( WTUNTV, NMDIM*KMAX )
      CALL RESET( WTUNTD, NMDIM*KMAX )
      CALL RESET( WTUNTT, NMDIM*KMAX )
      CALL RESET( WTUNTP, NMDIM      )
      CALL RESET( WTUNTQ, NMDIM*KMAX )
*
*     wave to grid
*
      IF( OMATV ) THEN
         WTUNTV( NMO(IC,M,L), KMATL) = UNTV
         CALL COPY( WAVOR, WTUNTV,  NMDIM*KMAX )
      ELSE IF ( OMATD ) THEN
         WTUNTD( NMO(IC,M,L), KMATL) = UNTD
         CALL COPY( WADIV, WTUNTD,  NMDIM*KMAX )
      ENDIF
      IF( OMATT ) THEN
         WTUNTT( NMO(IC,M,L), KMATL) = UNTT
         CALL COPY( WAT, WTUNTT,  NMDIM*KMAX )
      ENDIF
      IF( OMATP ) THEN
         WTUNTP( NMO(IC,M,L)       ) = UNTP
         CALL COPY( WAPI, WTUNTP,  NMDIM     )
      ENDIF
      IF( OMATQ ) THEN
         WTUNTQ( NMO(IC,M,L), KMATL) = UNTQ
         CALL COPY( WAQ, WTUNTQ,  NMDIM*KMAX )
      ENDIF
*
      CALL COPY( WBVOR , WAVOR , NMDIM*KMAX )
      CALL COPY( WBDIV , WADIV , NMDIM*KMAX )
      CALL COPY( WBT   , WAT   , NMDIM*KMAX )
      CALL COPY( WBPI  , WAPI  , NMDIM      )
      CALL COPY( WBQ   , WAQ   , NMDIM*KMAX )
*
      DO 2000 ISTP = 1, NSTP
*
*    << perturbation wave -> grid >>
*
         CALL WAVE2GRID
     O        ( GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     O          GAVOR , GADIV ,
     I          WAVOR , WADIV , WAT   , WAPI  , WAQ   ,
     W          WDX   , WDY                              )
*
*  << tendency term >>
*
         CALL DYNTRM_MAT        !! dynamics terms
     O         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     I           GAVOR , GADIV , 
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   , 
     I           DELT  , GDZS  , SDTOP , SDBTM , OSLNOID,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA         )
*
*  << time integration >>
*
         CALL DYNSTP_WAVE
     M         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     M           WAVOR , WADIV , WAT   , WAPI  , WAQ   ,
     M           WBVOR , WBDIV , WBT   , WBPI  , WBQ   ,
     I           DELT  , 
     I           DIFV  , DIFD  , DIFT  , DIFQ  , 
     I           FLAPLA, TDMPC ,
     I           WPHIS , GG    , TMTX  , WHMTX , GCMTX  )

 2000 CONTINUE
*
*     write wave comp. with unity
*
      WRITE( 6, *) ' @@@ WRITE DOWN MATRIX FILE : m=',
     &     m,' l=',l,'ic=',ic,'@@@'

      IF( M .EQ. 0 ) THEN
         LSTR = 1
         DO 4000 K = 1, KMAX
            NX = 0
            DO 4100 LL = LSTR, LEND
               NX = NX + 1
               WXVOR( NX, K, M) =  WTVOR( NMO(1,M,LL), K)
               WXDIV( NX, K, M) =  WTDIV( NMO(1,M,LL), K)
               WXTMP( NX, K, M) =  WTT( NMO(1,M,LL), K)
               WXSPH( NX, K, M) =  WTQ( NMO(1,M,LL), K)
 4100       CONTINUE
 4000    CONTINUE
      ELSE
         LSTR = 0
         DO 4200 K = 1, KMAX
            NX = 0
            DO 4300 LL = LSTR, LEND
               NX = NX + 1
               WXVOR( NX, K, M) =  WTVOR( NMO(1,M,LL), K)
               WXDIV( NX, K, M) =  WTDIV( NMO(1,M,LL), K)
               WXTMP( NX, K, M) =  WTT( NMO(1,M,LL), K)
               WXSPH( NX, K, M) =  WTQ( NMO(1,M,LL), K)
               NX = NX + 1
               WXVOR( NX, K, M) =  WTVOR( NMO(2,M,LL), K)
               WXDIV( NX, K, M) =  WTDIV( NMO(2,M,LL), K)
               WXTMP( NX, K, M) =  WTT( NMO(2,M,LL), K)
               WXSPH( NX, K, M) =  WTQ( NMO(2,M,LL), K)
 4300       CONTINUE
 4200    CONTINUE
      ENDIF

      NX = 0
      DO 4400 LL = LSTR, LEND
         NX = NX + 1
         WXPI( NX, M ) =  WTPI( NMO(1,M,LL) )
         IF( M .NE. 0 ) THEN
            NX = NX + 1
            WXPI( NX, M ) =  WTPI( NMO(2,M,LL) )
         ENDIF
 4400 CONTINUE
#ifdef OPT_CLASSIC
      WRITE( 77 ) ((WXVOR(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXDIV(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXTMP(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ( WXPI(IW,M)    ,IW=1,NX)
#else
      WRITE( 77 ) ((WXVOR(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXDIV(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXTMP(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ( WXPI(IW,M)    ,IW=1,NX),
     $            ((WXSPH(IW,IS,M),IW=1,NX),IS=1,KMAX)
#endif
*
      GOTO 222
*
 1999 RETURN
      END
**********************************************************************
      SUBROUTINE DYNMCS_MAT_R    !!  dynamics, adding R
     I         ( RFACT   )
*
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zhdim.F"                /* # of char. in string   */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
*
*   [INPUT]
      REAL*8     RFACT          !! factor for R used for AIM
*
*   [INTERNAL WORK] 
      REAL*8     GDU ( IJDIM, KMAX      )   !! BS westerly u (dummy)
*
      REAL*8     WTUNTV( NMDIM, KMAX   )  !! unit matrix
      REAL*8     WTUNTD( NMDIM, KMAX   )  !! unit matrix
      REAL*8     WTUNTT( NMDIM, KMAX   )  !! unit matrix
      REAL*8     WTUNTP( NMDIM         )  !! unit matrix 
      REAL*8     WTUNTQ( NMDIM, KMAX   )  !! unit matrix
*
      REAL*8     WRVOR ( NMDIM, KMAX   )  !! vorticity
      REAL*8     WRDIV ( NMDIM, KMAX   )  !! divergence D
      REAL*8     WRT   ( NMDIM, KMAX   )  !! temperature
      REAL*8     WRPI  ( NMDIM         )  !! logPs pi
      REAL*8     WRQ   ( NMDIM, KMAX   )  !! humidity
*
      REAL*8     WXVOR ( NMDIM, KMAX, 0:MMAX ) !! work
      REAL*8     WXDIV ( NMDIM, KMAX, 0:MMAX ) !! work
      REAL*8     WXTMP ( NMDIM, KMAX, 0:MMAX ) !! work
      REAL*8     WXPI  ( NMDIM      , 0:MMAX ) !! work
      REAL*8     WXSPH ( NMDIM, KMAX, 0:MMAX ) !! work

      INTEGER    IFPAR, JFPAR
      INTEGER    NM
      INTEGER    NX
      INTEGER    IW, IS
      INTEGER    L, IC, LSTR, LL, K, MM
*
*   [INTERNAL SAVE] 
      REAL*8     DIFV  ( NMDIM, KMAX   )  !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX   )  !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX   )  !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX   )  !! h-diff. coef. of water 
      REAL*8     DIFP  ( NMDIM         )  !! h-diff. coef (pi)

      REAL*8     RF

      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER    M, LEND, LEND0
      INTEGER    NXW( 0:MMAX )

      SAVE       DIFV, DIFD, DIFT, DIFQ, DIFP
      SAVE       NMO
      SAVE       RF
      SAVE       M, LEND, LEND0, NXW
*     
*   [INTERNAL PARM] 
      REAL*8     UNTV           !! unity for vorticity
      REAL*8     UNTD           !! unity for divergence
      REAL*8     UNTT           !! unity for temperature
      REAL*8     UNTP           !! unity for log(Ps)
      REAL*8     UNTQ           !! unity for humidity

      INTEGER    KMATW          !! position of adding unity in wave
      INTEGER    KMATL          !! position of adding unity in sigma

      CHARACTER  AMATF *(NFILN) !! file name for the matrix column
      CHARACTER  BMATF *(NFILN) !! file name for the matrix column

      LOGICAL    OMATV          !! add unity to the vorticity?
      LOGICAL    OMATD          !! add unity to the divergence?
      LOGICAL    OMATT          !! add unity to the temperature?
      LOGICAL    OMATP          !! add unity to the sfc. pressure?
      LOGICAL    OMATQ          !! add unity to the temperature?
      LOGICAL    OFIRST
      LOGICAL    OVWRT

      SAVE       OMATV, OMATD, OMATT, OMATP, OMATQ
      SAVE       KMATW, KMATL

      NAMELIST  /NMAMAT/ AMATF, 
     $           OMATV, OMATD, OMATT, OMATP, OMATQ, KMATW, KMATL
      NAMELIST  /NMBMAT/ BMATF

      DATA       AMATF  / '     ' /
      DATA       BMATF  / '     ' /
      DATA       OFIRST / .TRUE.  /
      DATA       OVWRT  / .TRUE.  /
      DATA       OMATV  / .FALSE. /
      DATA       OMATD  / .FALSE. /
      DATA       OMATT  / .FALSE. /
      DATA       OMATP  / .FALSE. /
      DATA       OMATQ  / .FALSE. /
      DATA       KMATW  / 0       /
      DATA       KMATL  / 0       /
*
      DATA       UNTV   / 1.0D-7 /
      DATA       UNTD   / 1.0D-7 /
      DATA       UNTT   / 1.0D-3 /
      DATA       UNTP   / 1.0D-5 /
      DATA       UNTQ   / 1.0D-6 /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADMN_MAT_R: ADDING R 05/07/28'
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMAMAT, END=290 )
 290     WRITE  ( JFPAR, NMAMAT )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMBMAT, END=390 )
 390     WRITE  ( JFPAR, NMBMAT )
         OFIRST = .FALSE.
*
         CALL DSETNM
     O         ( NMO    )
         CALL SPSTUP      !! spherical harmonic functions
         CALL RESET( GDU , IJDIM*KMAX )
         CALL DSETDF      !! horizontal diffusion
     O          ( DIFV  , DIFD  , DIFT  , DIFQ  , DIFP ,
     I            GDU     )
*
         RF = -1.D0 * RFACT
*
*     input/output files for the matrix column
*
         IF( AMATF(1:1) .EQ. ' ' ) THEN
            WRITE( 6, *) '    ### NO L MATRIX FILE SPECIFIED ### '
            CALL XABORT( 1 )
            RETURN
         ENDIF
         OPEN( 77, FILE = AMATF, FORM='UNFORMATTED', STATUS='OLD')
         WRITE( 6, *) '    $$$ ORIGINAL MATRIX FILE:', AMATF,' $$$'
*
         IF( BMATF(1:1) .EQ. ' ' ) THEN
            WRITE( 6, *) '    ### NO L+R MATRIX FILE SPECIFIED ### '
            CALL XABORT( 1 )
            RETURN
         ENDIF
         OPEN( 88, FILE = BMATF, FORM='UNFORMATTED', STATUS='UNKNOWN')
         WRITE( 6, *) '    $$$ MODIFIED MATRIX FILE:', BMATF,' $$$'

         M = KMATW
         LEND0 = MIN( LMAX, NMAX-M)

         DO 7000 MM = 0, MMAX
            IF( MM .EQ. 0 ) THEN
               LSTR = 1
               NXW( MM ) = LEND0 - LSTR + 1
            ELSE
               LSTR = 0
               NXW( MM ) = ( LEND0 - LSTR + 1 ) * 2
            ENDIF
 7000    CONTINUE
*
      ENDIF
*
*     loop for l
*
      L = -1
  333 LEND = LEND0
      L = L + 1
      IF( L .GT. LEND ) THEN
         CLOSE( 77 )
         GOTO 2999
      ENDIF
      IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 333 !! global mean
*
*     loop for Re/Im
*
      IC = 0
  444 LEND = LEND0
      IC = IC + 1
      IF( IC .GT. 2 ) GOTO 333
      IF( M .EQ. 0 .AND. IC .EQ. 2 ) GOTO 333 !! imaginary for zonal comp.
*
*     unit perturbation
*
      CALL RESET( WTUNTV, NMDIM*KMAX )
      CALL RESET( WTUNTD, NMDIM*KMAX )
      CALL RESET( WTUNTT, NMDIM*KMAX )
      CALL RESET( WTUNTP, NMDIM      )
      CALL RESET( WTUNTQ, NMDIM*KMAX )
*
*     wave to grid
*
      IF( OMATV ) THEN
         WTUNTV( NMO(IC,M,L), KMATL) = UNTV
      ELSE IF ( OMATD ) THEN
         WTUNTD( NMO(IC,M,L), KMATL) = UNTD
      ENDIF
      IF( OMATT ) THEN
         WTUNTT( NMO(IC,M,L), KMATL) = UNTT
      ENDIF
      IF( OMATP ) THEN
         WTUNTP( NMO(IC,M,L)       ) = UNTP
      ENDIF
      IF( OMATQ ) THEN
         WTUNTQ( NMO(IC,M,L), KMATL) = UNTQ
      ENDIF
*
*     make vector -RX^n
*
      DO 7100 K = 1, KMAX
         DO 7200 NM = 1, NMDIM
            WRVOR( NM,K ) = RF * DIFV( NM,K ) * WTUNTV( NM,K )
            WRDIV( NM,K ) = RF * DIFD( NM,K ) * WTUNTD( NM,K )
            WRT  ( NM,K ) = RF * DIFT( NM,K ) * WTUNTT( NM,K )
            WRQ  ( NM,K ) = RF * DIFQ( NM,K ) * WTUNTQ( NM,K )
 7200    CONTINUE
 7100 CONTINUE
      DO 7300 NM = 1, NMDIM
         WRPI( NM ) = RF * DIFP( NM ) * WTUNTP( NM )
 7300 CONTINUE
*
*     write wave comp. with unity
*
      WRITE( 6, *) ' @@@ WRITE DOWN MATRIX FILE : m=',
     &     m,' l=',l,'ic=',ic,'@@@'

      CALL CLCSTR ( '     READ' )
      NX = NXW( M )
#ifdef OPT_CLASSIC
      READ( 77 )  ((WXVOR(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXDIV(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXTMP(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ( WXPI(IW,M)    ,IW=1,NX)
#else
      READ( 77 )  ((WXVOR(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXDIV(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXTMP(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ( WXPI(IW,M)    ,IW=1,NX),
     $            ((WXSPH(IW,IS,M),IW=1,NX),IS=1,KMAX)
#endif
      CALL CLCEND ( '     READ' )

      IF( M .EQ. 0 ) THEN
         LSTR = 1
         DO 8000 K = 1, KMAX
            NX = 0
            DO 8100 LL = LSTR, LEND
               NX = NX + 1
               WXVOR( NX, K, M) =  WXVOR( NX, K, M )
     &                           + WRVOR( NMO(1,M,LL), K)
               WXDIV( NX, K, M) =  WXDIV( NX, K, M )
     &                           + WRDIV( NMO(1,M,LL), K)
               WXTMP( NX, K, M) =  WXTMP( NX, K, M )
     &                           + WRT( NMO(1,M,LL), K)
               WXSPH( NX, K, M) =  WXSPH( NX, K, M )
     &                           + WRQ( NMO(1,M,LL), K)
 8100       CONTINUE
 8000    CONTINUE
      ELSE
         LSTR = 0
         DO 8200 K = 1, KMAX
            NX = 0
            DO 8300 LL = LSTR, LEND
               NX = NX + 1
               WXVOR( NX, K, M) =  WXVOR( NX, K, M )
     &                           + WRVOR( NMO(1,M,LL), K)
               WXDIV( NX, K, M) =  WXDIV( NX, K, M )
     &                           + WRDIV( NMO(1,M,LL), K)
               WXTMP( NX, K, M) =  WXTMP( NX, K, M )
     &                           + WRT( NMO(1,M,LL), K)
               WXSPH( NX, K, M) =  WXSPH( NX, K, M )
     &                           + WRQ( NMO(1,M,LL), K)
               NX = NX + 1
               WXVOR( NX, K, M) =  WXVOR( NX, K, M )
     &                           + WRVOR( NMO(2,M,LL), K)
               WXDIV( NX, K, M) =  WXDIV( NX, K, M )
     &                           + WRDIV( NMO(2,M,LL), K)
               WXTMP( NX, K, M) =  WXTMP( NX, K, M )
     &                           + WRT( NMO(2,M,LL), K)
               WXSPH( NX, K, M) =  WXSPH( NX, K, M )
     &                           + WRQ( NMO(2,M,LL), K)
 8300       CONTINUE
 8200    CONTINUE
      ENDIF

      NX = 0
      DO 8400 LL = LSTR, LEND
         NX = NX + 1
         WXPI( NX, M ) =  WXPI( NX, M )
     &                  + WRPI( NMO(1,M,LL) )
         IF( M .NE. 0 ) THEN
            NX = NX + 1
            WXPI( NX, M ) =  WXPI( NX, M )
     &                     + WRPI( NMO(2,M,LL) )
         ENDIF
 8400 CONTINUE
      CALL CLCSTR ( '     WRITE' )
#ifdef OPT_CLASSIC
      WRITE( 88 ) ((WXVOR(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXDIV(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXTMP(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ( WXPI(IW,M)    ,IW=1,NX)
#else
      WRITE( 88 ) ((WXVOR(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXDIV(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ((WXTMP(IW,IS,M),IW=1,NX),IS=1,KMAX),
     $            ( WXPI(IW,M)    ,IW=1,NX),
     $            ((WXSPH(IW,IS,M),IW=1,NX),IS=1,KMAX)
#endif
      CALL CLCEND ( '     WRITE' )
*
      GOTO 444
*
 2999 RETURN
      END
