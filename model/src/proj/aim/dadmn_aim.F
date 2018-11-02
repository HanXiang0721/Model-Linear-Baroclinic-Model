* PACKAGE DADMN !! dynamics: main routine
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/03(numaguti) tracer
*         96/06/06(numaguti) remove FIXMAS/MASFIX
*         01/06/15(hiro)     PWM incl. moisture
*         05/03/30(hiro)     add additional diffusion R
*         05/05/26(hiro)     AIM for baroclinic model
**********************************************************************
      SUBROUTINE DYNMCS_AIM    !!  dynamics
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
      INTEGER     MAXN
#ifdef OPT_CLASSIC
      PARAMETER ( MAXN = 2*NMAX*(KMAX*3+1) ) !! dry
#else
      PARAMETER ( MAXN = 2*NMAX*(KMAX*4+1) ) !! moist
#endif
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
*   [INTERNAL WORK] 
      REAL*8     GAU ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAV ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAT ( IJDIM, KMAX       )  !! perturbation temperature 
      REAL*8     GAPI( IJDIM             )  !! perturbation pi
      REAL*8     GAQ ( IJDIM, KMAX       )  !! perturbation humidity
      REAL*8     GAVOR ( IJDIM, KMAX     )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX     )  !! divergence D 

      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work

      REAL*8     WTVOR ( NMDIM, KMAX   )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX   )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX   )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM         )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX   )  !! tendency of humidity

      REAL*8     WTVORZ( NMDIM, KMAX   )  !! tendency of vorticity
      REAL*8     WTDIVZ( NMDIM, KMAX   )  !! tendency of divergence D
      REAL*8     WTTZ  ( NMDIM, KMAX   )  !! tendency of temperature
      REAL*8     WTPIZ ( NMDIM         )  !! logPs pi tendency
      REAL*8     WTQZ  ( NMDIM, KMAX   )  !! tendency of humidity
*
      REAL*8     WAVOR ( NMDIM, KMAX   )  !! vorticity
      REAL*8     WADIV ( NMDIM, KMAX   )  !! divergence D
      REAL*8     WAT   ( NMDIM, KMAX   )  !! temperature
      REAL*8     WAPI  ( NMDIM         )  !! logPs pi
      REAL*8     WAQ   ( NMDIM, KMAX   )  !! humidity
*
      REAL*8     WBVOR ( NMDIM, KMAX   )  !! vorticity
      REAL*8     WBDIV ( NMDIM, KMAX   )  !! divergence D
      REAL*8     WBT   ( NMDIM, KMAX   )  !! temperature
      REAL*8     WBPI  ( NMDIM         )  !! logPs pi
      REAL*8     WBQ   ( NMDIM, KMAX   )  !! humidity
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
*
      REAL*8     XP( MAXN,0:MMAX )           !! correction vector -(La-R)
      REAL*8     DNORM                       !! norm diff. from step n-1
      REAL*8     RDNORM                      !! lambda
*
      INTEGER    IFPAR, JFPAR
      INTEGER    NM
      INTEGER    NX
      INTEGER    NXW( 0:MMAX )
      INTEGER    M, L, LSTR, LEND, K
      INTEGER    NSFCS                       !! no. of forcing

      LOGICAL    OINITX
      LOGICAL    OLASTX
*
*   [INTERNAL SAVE] 
      REAL*8     DIFV  ( NMDIM, KMAX   )  !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX   )  !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX   )  !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX   )  !! h-diff. coef. of water
      REAL*8     DIFP  ( NMDIM         )  !! h-diff. coef (pi)

      REAL*8     WPHIS ( NMDIM  )         !! surface G.P.
*
      REAL*8     SDTOP ( IJDIM         )  !! coefficient for diss.
      REAL*8     SDBTM ( IJDIM         )  !! coefficient for diss.
*
      REAL*8     DNORM1                   !! norm diff. at n=1
      REAL*8     RF
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
      REAL*8     TDMPC                    !! damping coeff. for temp.
*
      REAL*8     CORIOL( IJDIM )          !! Coriolis factor f
      REAL*8     FLAPLA( NMDIM )          !! Laplacian factor
*
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER    IUNITM, IUNITF, IUNITR
      INTEGER    ITENUM
*
      SAVE       DIFV, DIFD, DIFT, DIFQ, DIFP
      SAVE       WPHIS
      SAVE       SDTOP, SDBTM
      SAVE       DNORM1
      SAVE       NMO
      SAVE       IUNITM, IUNITF, IUNITR
      SAVE       ITENUM

      SAVE       SALPHA, SBETA, SKAPPA, ATF, BTF, TBAR
      SAVE       GG, TMTX, WHMTX, GCMTX
      SAVE       TDMPC

      SAVE       CORIOL, FLAPLA
      SAVE       RF
*     
*   [INTERNAL PARM] 
      REAL*8     RFACT          !! factor for R used for AIM
      REAL*8     EPS            !! threshold for lambda
      CHARACTER  AMATF *(NFILN) !! file name for the matrix  (input)
      CHARACTER  FSFRC *(NFILN) !! file name for the forcing (input)
      CHARACTER  FRSP  *(NFILN) !! file name for the response (output)
      INTEGER    NWTRN          !! zonal wavenumber truncation
      INTEGER    NITEMX         !! maxinum iteration number
      INTEGER    NFFST          !! first line of forcing
      INTEGER    NFLST          !! last line of forcing
      LOGICAL    OFIRST
      LOGICAL    OFMULT         !! set multiple forcing
      LOGICAL    OINIT
      LOGICAL    OLAST
      LOGICAL    OFDUMP         !! force dump at every step
      LOGICAL    OSLNOID        !! include solenoidal term in vor.

      NAMELIST  /NMAMAT/ AMATF, FSFRC, OFMULT, NFFST, NFLST, NWTRN
      NAMELIST  /NMRSP/  FRSP, OINIT, OFDUMP
      NAMELIST  /NMRDIF/ RFACT
      NAMELIST  /NMAIM/  NITEMX, EPS
      NAMELIST  /NMSOLE/ OSLNOID

      DATA       AMATF  / '     ' /
      DATA       FSFRC  / '     ' /
      DATA       FRSP   / '     ' /
      DATA       NFFST  / 1       /
      DATA       NFLST  / 1       /
      DATA       NWTRN  / MMAX    /
      DATA       OFIRST / .TRUE.  /
      DATA       OFMULT / .FALSE. /
      DATA       OINIT  / .TRUE.  /
      DATA       OLAST  / .FALSE. /
      DATA       OFDUMP / .FALSE. /
      DATA       OSLNOID / .TRUE. /
      DATA       RFACT  / 0.D0    /
      DATA       ITENUM / -1      /
      DATA       NITEMX / 5       /
      DATA       EPS    / 1.D-2   /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADMN_AIM: ITERATION STARTED 05/05/26'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMAMAT, END=190 )
 190     WRITE  ( JFPAR, NMAMAT )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMRSP, END=290 )
 290     WRITE  ( JFPAR, NMRSP )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMRDIF, END=390 )
 390     WRITE  ( JFPAR, NMRDIF )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMAIM, END=490 )
 490     WRITE  ( JFPAR, NMAIM )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSOLE, END=590 )
 590     WRITE  ( JFPAR, NMSOLE)
*
         OINITX = OINIT
         OLASTX = OLAST
         IF( OFDUMP ) OINITX = .TRUE.
         NSFCS = NFFST
*
         CALL DSETNM
     O         ( NMO    )
         CALL SPSTUP      !! spherical harmonic functions
         CALL G2W
     O         ( WPHIS ,
     I           GDZS  ,
     F           '    ', 'POS ', 1     )
         DO 200 NM = 1, NMDIM
CCC            WPHIS( NM ) = WPHIS( NM )*GRAV
            WPHIS( NM ) = 0.D0
  200    CONTINUE 
*
         CALL RESET( GAU  , IJDIM*KMAX )
         CALL RESET( GAV  , IJDIM*KMAX )
         CALL RESET( GAVOR, IJDIM*KMAX )
         CALL RESET( GADIV, IJDIM*KMAX )
         CALL RESET( GAPI , IJDIM      )
         CALL RESET( GAT  , IJDIM*KMAX )
         CALL RESET( GAQ  , IJDIM*KMAX )
*
         CALL RESET( WAVOR, NMDIM*KMAX )
         CALL RESET( WADIV, NMDIM*KMAX )
         CALL RESET( WAT  , NMDIM*KMAX )
         CALL RESET( WAPI , NMDIM      )
         CALL RESET( WAQ  , NMDIM*KMAX )
*
         CALL RESET( WBVOR, NMDIM*KMAX )
         CALL RESET( WBDIV, NMDIM*KMAX )
         CALL RESET( WBT  , NMDIM*KMAX )
         CALL RESET( WBPI , NMDIM      )
         CALL RESET( WBQ  , NMDIM*KMAX )
*
         CALL RESET( XP   , MAXN*(MMAX+1)  )
*
         CALL DSETDF      !! horizontal diffusion
     O          ( DIFV  , DIFD  , DIFT  , DIFQ  , DIFP ,
     I            GFU     )
*
         RF = RFACT
*
         CALL DSETVD      !! vertical difference/semi implicit
     O        ( SALPHA, SBETA , SKAPPA,
     O          ATF   , BTF   , TBAR  , 
     O          GG    , TMTX  , WHMTX , GCMTX ,
     I          GFT    )
*
         CALL RESET( CORIOL , IJDIM ) !! set f=0 for LaX
         CALL DSETLP
     O         ( FLAPLA )
         CALL DSETBTD
     O         ( SDTOP , SDBTM , TDMPC ,
     I           GFT     )
         CALL DSETVDF
*
*     input file for the inverse matrix
*
         IF( AMATF(1:1) .EQ. ' ' ) THEN
            WRITE( 6, *) '    ### NO MATRIX FILE SPECIFIED ### '
            CALL XABORT( 1 )
            RETURN
CC         ELSEIF( FSFRC(1:1) .EQ. ' ' ) THEN
CC            WRITE( 6, *) '    ### NO FORCING FILE SPECIFIED ### '
CC            CALL XABORT( 1 )
CC            RETURN
         ELSEIF( FRSP(1:1) .EQ. ' ' ) THEN
            WRITE( 6, *) '    ### NO RESPONSE FILE SPECIFIED ### '
            CALL XABORT( 1 )
            RETURN
         ENDIF
         IUNITM = 77
         IUNITR = 79
         OPEN( IUNITM, FILE = AMATF, 
     &         FORM='UNFORMATTED', STATUS='UNKNOWN')
         OPEN( IUNITR, FILE = FRSP, 
     &         FORM='UNFORMATTED', STATUS='UNKNOWN')
         IF( FSFRC(1:1) .EQ. ' ' ) THEN
            IUNITF = -99
         ELSE
            IUNITF = 78
            OPEN( IUNITF, FILE = FSFRC, 
     &           FORM='UNFORMATTED', STATUS='UNKNOWN')
            WRITE( 6, *) '    $$$ FORCING FILE:', FSFRC,' $$$'
         ENDIF
         WRITE( 6, *) '    $$$ INV. MATRIX FILE:', AMATF,' $$$'
         WRITE( 6, *) '    $$$ RESPONSE FILE:', FRSP,' $$$'
*
      ENDIF
*
*  << tendency term >>
*
 9000 ITENUM = ITENUM + 1
*
*     make vector -RX^n
*
      DO 1000 K = 1, KMAX
         DO 1100 NM = 1, NMDIM
            WRVOR( NM,K ) = RF * DIFV( NM,K ) * WAVOR( NM,K )
            WRDIV( NM,K ) = RF * DIFD( NM,K ) * WADIV( NM,K )
            WRT  ( NM,K ) = RF * DIFT( NM,K ) * WAT  ( NM,K )
            WRQ  ( NM,K ) = RF * DIFQ( NM,K ) * WAQ  ( NM,K )
 1100    CONTINUE
 1000 CONTINUE
      DO 1200 NM = 1, NMDIM
         WRPI( NM ) = RF * DIFP( NM ) * WAPI( NM )
 1200 CONTINUE
*
*  << dynamic terms >>
*
      CALL CLCSTR ( 'DYNTRM' )
      CALL CLCSTR ( '  TERM_FULL' )
*
      CALL DYNTRM_AIM_FULL      !! dynamics terms with full BS
     O         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     I           GAVOR , GADIV , 
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           DELT  , GDZS  , SDTOP , SDBTM , OSLNOID,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA         )
      CALL CLCEND ( '  TERM_FULL' )
*
      CALL CLCSTR ( '  TERM_ZM' )
      CALL DYNTRM_MAT           !! dynamics terms with ZMBS
     O         ( WTVORZ, WTDIVZ, WTTZ  , WTPIZ , WTQZ  ,
     I           GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     I           GAVOR , GADIV , 
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           DELT  , GDZS  , SDTOP , SDBTM , OSLNOID,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA         )
      CALL CLCEND ( '  TERM_ZM' )
      CALL CLCEND ( 'DYNTRM' )
*
*  << semi-implicit >>
*
      CALL CLCSTR ( 'TINTGR' ) 
      CALL CLCSTR ( '  TINTGR_FULL' ) 
      CALL TINTGR
     M         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     M           WAVOR , WADIV , WAT   , WAPI  , WAQ   ,
     I           DELT  , WPHIS , 
     I           TDMPC ,
     C           DIFV  , DIFD  , DIFT  , DIFQ  , 
     C           FLAPLA, GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDY                            )
      CALL CLCEND ( '  TINTGR_FULL' ) 
*
      CALL CLCSTR ( '  TINTGR_ZM' ) 
      CALL TINTGR
     M         ( WTVORZ, WTDIVZ, WTTZ  , WTPIZ , WTQZ  ,
     M           WBVOR , WBDIV , WBT   , WBPI  , WBQ   ,
     I           DELT  , WPHIS , 
     I           TDMPC ,
     C           DIFV  , DIFD  , DIFT  , DIFQ  , 
     C           FLAPLA, GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDY                            )
      CALL CLCEND ( '  TINTGR_ZM' )
      CALL CLCEND ( 'TINTGR' )
*
*     tendency due to waves
*
      DO 500 M = 0, MMAX
         LEND = MIN( LMAX, NMAX-M)
         DO 300 K = 1, KMAX
            NX = 0

            LSTR = 0
            IF( M .EQ. 0 ) LSTR = 1
            DO 310 L = LSTR, LEND
               NX = NX + 1
               WXVOR( NX, K, M) =   WTVOR ( NMO(1,M,L), K)
     &                            - WTVORZ( NMO(1,M,L), K)
     &                            + WRVOR ( NMO(1,M,L), K)
               WXDIV( NX, K, M) =   WTDIV ( NMO(1,M,L), K)
     &                            - WTDIVZ( NMO(1,M,L), K)
     &                            + WRDIV ( NMO(1,M,L), K)
               WXTMP( NX, K, M) =   WTT   ( NMO(1,M,L), K)
     &                            - WTTZ  ( NMO(1,M,L), K)
     &                            + WRT   ( NMO(1,M,L), K)
               WXSPH( NX, K, M) =   WTQ   ( NMO(1,M,L), K)
     &                            - WTQZ  ( NMO(1,M,L), K)
     &                            + WRQ   ( NMO(1,M,L), K)
*
               IF( M .NE. 0 ) THEN
                  NX = NX + 1
                  WXVOR( NX, K, M) =  WTVOR ( NMO(2,M,L), K)
     &                              - WTVORZ( NMO(2,M,L), K)
     &                              + WRVOR ( NMO(2,M,L), K)
                  WXDIV( NX, K, M) =  WTDIV ( NMO(2,M,L), K)
     &                              - WTDIVZ( NMO(2,M,L), K)
     &                              + WRDIV ( NMO(2,M,L), K)
                  WXTMP( NX, K, M) =  WTT   ( NMO(2,M,L), K)
     &                              - WTTZ  ( NMO(2,M,L), K)
     &                              + WRT   ( NMO(2,M,L), K)
                  WXSPH( NX, K, M) =  WTQ   ( NMO(2,M,L), K)
     &                              - WTQZ  ( NMO(2,M,L), K)
     &                              + WRQ   ( NMO(2,M,L), K)
               ENDIF
 310        CONTINUE
 300     CONTINUE
         NX = 0
         DO 320 L = LSTR, LEND
            NX = NX + 1
            WXPI( NX, M ) =  WTPI ( NMO(1,M,L) )
     &                     - WTPIZ( NMO(1,M,L) )
     &                     + WRPI ( NMO(1,M,L) )
            IF( M .NE. 0 ) THEN
               NX = NX + 1
               WXPI( NX, M ) =  WTPI ( NMO(2,M,L) )
     &                        - WTPIZ( NMO(2,M,L) )
     &                        + WRPI ( NMO(2,M,L) )
            ENDIF
 320     CONTINUE

         NXW( M ) = NX

  500 CONTINUE

      DO 550 M = 0, MMAX
         NX = 0

         DO 600 K = 1, KMAX
            DO 610 NM = 1, NXW( M )
               NX = NX + 1
               XP( NX,M ) = WXVOR( NM,K,M )
 610        CONTINUE
 600     CONTINUE
         DO 620 K = 1, KMAX
            DO 630 NM = 1, NXW( M )
               NX = NX + 1
               XP( NX,M ) = WXDIV( NM,K,M )
 630        CONTINUE
 620     CONTINUE
         DO 640 K = 1, KMAX
            DO 650 NM = 1, NXW( M )
               NX = NX + 1
               XP( NX,M ) = WXTMP( NM,K,M )
 650        CONTINUE
 640     CONTINUE
         DO 660 NM = 1, NXW( M )
            NX = NX + 1
            XP( NX,M ) = WXPI( NM,M )
 660     CONTINUE
#ifndef OPT_CLASSIC
         DO 670 K = 1, KMAX
            DO 680 NM = 1, NXW( M )
               NX = NX + 1
               XP( NX,M ) = WXSPH( NM,K,M )
 680        CONTINUE
 670     CONTINUE
#endif
*
 550  CONTINUE
*
*     read inverse matrix and make covariance with forcing
*
      CALL CLCSTR ( 'COV' ) 
      CALL RDSFRC
     M         ( XP    ,
     I           MAXN  , IUNITF, NSFCS , DELT  , WPHIS , 
     C           FLAPLA, GG    , TMTX  , WHMTX , GCMTX  )
      CALL COV
     O     ( WAVOR  , WADIV  , WAT    , WAPI   , WAQ    ,
     I       IUNITM , NWTRN  , XP                        )
      CALL CLCEND ( 'COV' ) 
*
*     advance the variable arrays
*
      CALL COPY( WBVOR , WAVOR , NMDIM*KMAX )
      CALL COPY( WBDIV , WADIV , NMDIM*KMAX )
      CALL COPY( WBT   , WAT   , NMDIM*KMAX )
      CALL COPY( WBPI  , WAPI  , NMDIM      )
      CALL COPY( WBQ   , WAQ   , NMDIM*KMAX )
*
      CALL WAVE2GRID            !! perturbation wave -> grid
     O         ( GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     O           GAVOR , GADIV ,
     I           WAVOR , WADIV , WAT   , WAPI  , WAQ   ,
     W           WDX   , WDY                              )
*     
*     dump response in physical space
*
      CALL CLCSTR ( 'DUMP' ) 
      CALL DUMPRSP    
     O     ( DNORM ,
     I       GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     I       WAVOR , WADIV , WAT   , WAPI  , WAQ   ,
     I       GFU   , GFV   , GFT   , GFPS  , GFQ   , GDZS  , 
     I       IUNITR, OINITX, OLASTX                          )
      CALL CLCEND ( 'DUMP' ) 
*
      IF( ITENUM .EQ. 0 ) THEN
         RDNORM = 9.99D35
         OINITX = .FALSE.
         IF( OFDUMP ) OINITX = .TRUE.
      ELSEIF( ITENUM .EQ. 1 ) THEN
         RDNORM = 1.D0
         DNORM1 = DNORM
         IF( DNORM1 .EQ. 0.D0 ) THEN
            WRITE( JFPAR,* ) ' ### NO AIM: ', ITENUM
            GOTO 9100
         ENDIF
      ELSE
         RDNORM = DNORM / DNORM1
      ENDIF
*
      WRITE( JFPAR,* ) 
     &     ' ===== ITE: ', ITENUM,
     &     ' DNORM=', DNORM,
     &     ' LAMBDA=', RDNORM

      IF( ITENUM .EQ. NITEMX ) THEN
         IF( OFMULT )
     &        WRITE( JFPAR,* ) ' @@@ FORCING NO. = ',NSFCS
         WRITE( JFPAR,* ) ' ### AIM NOT CONVERGED AT: ', NITEMX
         OLASTX = .TRUE.
         IF( OFDUMP ) THEN
            OINITX = .FALSE.
            OLASTX = .FALSE.
         ENDIF
         CALL DUMPRSP    
     O        ( DNORM ,
     I          GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     I          WAVOR , WADIV , WAT   , WAPI  , WAQ   ,
     I          GFU   , GFV   , GFT   , GFPS  , GFQ   , GDZS  , 
     I          IUNITR, OINITX, OLASTX                          )
         GOTO 9100
      ENDIF
      IF( RDNORM .LT. EPS ) THEN
         IF( OFMULT )
     &        WRITE( JFPAR,* ) ' @@@ FORCING NO. = ',NSFCS
         WRITE( JFPAR,* ) ' @@@ AIM CONVERGED AT: ', ITENUM
         OLASTX = .TRUE.
         IF( OFDUMP ) THEN
            OINITX = .FALSE.
            OLASTX = .FALSE.
         ENDIF
         CALL DUMPRSP    
     O        ( DNORM ,
     I          GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     I          WAVOR , WADIV , WAT   , WAPI  , WAQ   ,
     I          GFU   , GFV   , GFT   , GFPS  , GFQ   , GDZS  , 
     I          IUNITR, OINITX, OLASTX                          )
         GOTO 9100
      ENDIF

      GOTO 9000
*
*
 9100 IF( OFMULT ) THEN

         OINITX = OINIT
         OLASTX = OLAST
         IF( OFDUMP ) OINITX = .TRUE.
         ITENUM = -1
         NSFCS = NSFCS + 1
         IF( NSFCS .GT. NFLST ) GOTO 9200

         CALL RESET( GAU  , IJDIM*KMAX )
         CALL RESET( GAV  , IJDIM*KMAX )
         CALL RESET( GAVOR, IJDIM*KMAX )
         CALL RESET( GADIV, IJDIM*KMAX )
         CALL RESET( GAPI , IJDIM      )
         CALL RESET( GAT  , IJDIM*KMAX )
         CALL RESET( GAQ  , IJDIM*KMAX )
*
         CALL RESET( WAVOR, NMDIM*KMAX )
         CALL RESET( WADIV, NMDIM*KMAX )
         CALL RESET( WAT  , NMDIM*KMAX )
         CALL RESET( WAPI , NMDIM      )
         CALL RESET( WAQ  , NMDIM*KMAX )
*
         CALL RESET( WBVOR, NMDIM*KMAX )
         CALL RESET( WBDIV, NMDIM*KMAX )
         CALL RESET( WBT  , NMDIM*KMAX )
         CALL RESET( WBPI , NMDIM      )
         CALL RESET( WBQ  , NMDIM*KMAX )
*
         CALL RESET( XP   , MAXN*(MMAX+1)  )

         CALL CLR_DUMPRSP

         GOTO 9000

      ENDIF
*
 9200 CLOSE( IUNITF )
      CLOSE( IUNITR )
*
      RETURN
      END
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
      INTEGER     MAXN2
#ifdef OPT_CLASSIC
      PARAMETER ( MAXN  = 2*NMAX*(KMAX*3+1)   ) !! dry
      PARAMETER ( MAXN2 = (NMAX+1)*(KMAX*3+1) ) !! dry
#else
      PARAMETER ( MAXN  = 2*NMAX*(KMAX*4+1)   ) !! moist
      PARAMETER ( MAXN2 = (NMAX+1)*(KMAX*4+1) ) !! moist
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
      INTEGER    I, J, K, M, L, LEND, LDA
      INTEGER    JSTR, JEND
      INTEGER    NM
*
*   [INTERNAL SAVE] 
      REAL*8     A1( MAXN*MAXN,0:MMAX/2 )        !! inverse linear matrix
      REAL*8     A2( MAXN2*MAXN2,MMAX/2+1:MMAX ) !! inverse linear matrix
CC      REAL*8, DIMENSION(:,:), ALLOCATABLE :: A1
CC      REAL*8, DIMENSION(:,:), ALLOCATABLE :: A2
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER    MVAR

      SAVE       A1, A2
      SAVE       MVAR
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
CC         ALLOCATE  ( A1( MAXN*MAXN, 0:MMAX/2 ) )
CC         ALLOCATE  ( A2( MAXN2*MAXN2, MMAX/2+1:MMAX ) )
*
         CALL DSETNM
     O         ( NMO    )
*
#ifdef OPT_CLASSIC
         MVAR = 3
#else
         MVAR = 4
#endif
*         
*     store inverse matrix and external forcing
*
         DO 1100 M = 0, NWTRN   !! zonal wave number
            L = NMAX - M + 1
            IF( M .EQ. 0 ) THEN
               L = L - 1
            ELSE
               L = 2 * L
            ENDIF
            LDA = L * ( MVAR * KMAX + 1 )
            LEND = MIN( LMAX, NMAX-M )

            write(6,*) '     M=',M, '  LDA=',LDA
            IF( M .LE. MMAX/2 ) THEN
               DO 1200 I = 1, LDA 
                  JSTR = (I-1)*LDA + 1
                  JEND = JSTR + LDA - 1
                  READ( IUNITM ) ( A1( J,M ), J = JSTR, JEND )
 1200          CONTINUE
            ELSE
               DO 1400 I = 1, LDA 
                  JSTR = (I-1)*LDA + 1
                  JEND = JSTR + LDA - 1
                  READ( IUNITM ) ( A2( J,M ), J = JSTR, JEND )
 1400          CONTINUE
            ENDIF

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
      DO 2000 M = 0, NWTRN      !! zonal wave number

         L = NMAX - M + 1
         IF( M .EQ. 0 ) THEN
            L = L - 1
         ELSE
            L = 2 * L
         ENDIF
         LDA = L * ( MVAR * KMAX + 1 )
         LEND = MIN( LMAX, NMAX-M )
*
         CALL CLCSTR ( '  COVIN' ) 
         CALL RESET( X,   MAXN            )

         IF( M .LE. MMAX/2 ) THEN
            DO 2100 I = 1, LDA 
               DO 2110 J = 1, LDA
                  K = J + (I-1) * LDA
                  X( I ) = X( I ) + A1( K,M ) * XP( J,M )
 2110          CONTINUE
 2100       CONTINUE
         ELSE
            DO 2200 I = 1, LDA 
               DO 2210 J = 1, LDA
                  K = J + (I-1) * LDA
                  X( I ) = X( I ) + A2( K,M ) * XP( J,M )
 2210          CONTINUE
 2200       CONTINUE
         ENDIF
         CALL CLCEND ( '  COVIN' ) 
*
         NM = 0
         IF( M .EQ. 0 ) THEN
*
            DO 3000 K = 1, KMAX !! vorticity
               DO 3100 L = 1, LEND !! meridional wave number
                  NM = NM + 1
                  I = NMO( 1,M,L )
                  WDVOR( I,K ) = WDVOR( I,K ) + X( NM )
 3100          CONTINUE
 3000       CONTINUE
*     
            DO 3200 K = 1, KMAX !! divergence
               DO 3300 L = 1, LEND  
                  NM = NM + 1
                  I = NMO( 1,M,L )
                  WDDIV( I,K ) = WDDIV( I,K ) + X( NM )
 3300          CONTINUE
 3200       CONTINUE
*     
            DO 3400 K = 1, KMAX !! temperature
               DO 3500 L = 1, LEND  
                  NM = NM + 1
                  I = NMO( 1,M,L )
                  WDT( I,K ) = WDT( I,K ) + X( NM )
 3500          CONTINUE
 3400       CONTINUE
*     
            DO 3600 L = 1, LEND !! ln Ps
               NM = NM + 1
               I = NMO( 1,M,L )
               WDPI( I ) = WDPI( I ) + X( NM )
 3600       CONTINUE
*     
#ifndef OPT_CLASSIC
            DO 3700 K = 1, KMAX !! vorticity
               DO 3800 L = 1, LEND !! meridional wave number
                  NM = NM + 1
                  I = NMO( 1,M,L )
                  WDQ( I,K ) = WDQ( I,K ) + X( NM )
 3800          CONTINUE
 3700       CONTINUE
#endif
*
         ELSE
*
            DO 4000 K = 1, KMAX !! vorticity
               DO 4100 L = 0, LEND !! meridional wave number
                  NM = NM + 1
                  I = NMO( 1,M,L )
                  WDVOR( I,K ) = WDVOR( I,K ) + X( NM )
                  NM = NM + 1
                  I = NMO( 2,M,L )
                  WDVOR( I,K ) = WDVOR( I,K ) + X( NM )
 4100          CONTINUE
 4000       CONTINUE
*     
            DO 4200 K = 1, KMAX !! divergence
               DO 4300 L = 0, LEND  
                  NM = NM + 1
                  I = NMO( 1,M,L )
                  WDDIV( I,K ) = WDDIV( I,K ) + X( NM )
                  NM = NM + 1
                  I = NMO( 2,M,L )
                  WDDIV( I,K ) = WDDIV( I,K ) + X( NM )
 4300          CONTINUE
 4200       CONTINUE
*     
            DO 4400 K = 1, KMAX !! temperature
               DO 4500 L = 0, LEND  
                  NM = NM + 1
                  I = NMO( 1,M,L )
                  WDT( I,K ) = WDT( I,K ) + X( NM )
                  NM = NM + 1
                  I = NMO( 2,M,L )
                  WDT( I,K ) = WDT( I,K ) + X( NM )
 4500          CONTINUE
 4400       CONTINUE
*     
            DO 4600 L = 0, LEND !! ln Ps
               NM = NM + 1
               I = NMO( 1,M,L )
               WDPI( I ) = WDPI( I ) + X( NM )
               NM = NM + 1
               I = NMO( 2,M,L )
               WDPI( I ) = WDPI( I ) + X( NM )
 4600       CONTINUE
*     
#ifndef OPT_CLASSIC
            DO 4700 K = 1, KMAX !! vorticity
               DO 4800 L = 0, LEND !! meridional wave number
                  NM = NM + 1
                  I = NMO( 1,M,L )
                  WDQ( I,K ) = WDQ( I,K ) + X( NM )
                  NM = NM + 1
                  I = NMO( 2,M,L )
                  WDQ( I,K ) = WDQ( I,K ) + X( NM )
 4800          CONTINUE
 4700       CONTINUE
#endif
*
         ENDIF
 2000 CONTINUE
*
*
*
      RETURN
      END
**********************************************************************
      SUBROUTINE DUMPRSP    !! dump response in physical space
     O         ( DNORM , 
     I           GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     I           WDVOR , WDDIV , WDT   , WDPI  , WDQ   ,
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   , GDZS  , 
     I           IUNITR, OINIT , OLAST                           )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zhdim.F"                /* # of char. in string   */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     DNORM                       !! norm diff. from step n-1
*
*   [INPUT] 
      REAL*8     GAU ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAV ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAT ( IJDIM, KMAX       )  !! perturbation temperature 
      REAL*8     GAQ ( IJDIM, KMAX       )  !! perturbation humidity
      REAL*8     GAPI( IJDIM             )  !! perturbation pi

      REAL*8     WDVOR ( NMDIM, KMAX      )  !! vorticity
      REAL*8     WDDIV ( NMDIM, KMAX      )  !! divergence D
      REAL*8     WDT   ( NMDIM, KMAX      )  !! temperature
      REAL*8     WDPI  ( NMDIM            )  !! logPs pi
      REAL*8     WDQ   ( NMDIM, KMAX      )  !! humidity
*
      REAL*8     GFU   ( IJDIM, KMAX      )  !! BS westerly u 
      REAL*8     GFV   ( IJDIM, KMAX      )  !! BS southern wind v
      REAL*8     GFT   ( IJDIM, KMAX      )  !! BS temperature
      REAL*8     GFPS  ( IJDIM            )  !! BS surface pressure
      REAL*8     GFQ   ( IJDIM, KMAX      )  !! BS  humidity

      REAL*8     GDZS  ( IJDIM            )  !! topography
      INTEGER    IUNITR                      !! file unit no. 

      LOGICAL    OINIT
      LOGICAL    OLAST
*
*   [INTERNAL WORK] 
      REAL*8     GDPSI ( IJDIM, KMAX     )  !! streamfunction
      REAL*8     GDCHI ( IJDIM, KMAX     )  !! velocity potential
      REAL*8     GDU ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GDV ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GDT ( IJDIM, KMAX       )  !! perturbation temperature 
      REAL*8     GDQ ( IJDIM, KMAX       )  !! perturbation humidity
      REAL*8     GDW ( IJDIM, KMAX       )  !! p-verlocity
      REAL*8     GDPS( IJDIM             )  !! perturbation sfc. pressure
      REAL*8     GDSLP( IJDIM            )  !! perturbation SLP

      REAL*8     GDTV( IJDIM, KMAX       )  !! virtual temperature 
      REAL*8     GDZ ( IJDIM, KMAX       )  !! geopotential height

      REAL*8     GDX ( IJDIM, KMAX       )  !! work
      REAL*8     GDY ( IJDIM, KMAX       )  !! work
      REAL*8     WDX ( NMDIM, KMAX       )  !! work
      REAL*8     WDY ( NMDIM, KMAX       )  !! work

      REAL*8     RMSE
      REAL*8     TLAPS, TVS, TV0, TVX

      REAL*4     GR4 ( IDIM-1, JDIM      )  !! work

      INTEGER    IFPAR, JFPAR
      INTEGER    IJ, K, NM
*
*   [INTERNAL SAVE] 
      REAL*8     GBU ( IJDIM, KMAX       )  !! perturbation wind at n-1
      REAL*8     GBV ( IJDIM, KMAX       )  !! perturbation wind at n-1
      REAL*8     GBT ( IJDIM, KMAX       )  !! perturbation temp. at n-1 
      REAL*8     GBPS( IJDIM             )  !! perturbation Ps at n-1
      REAL*8     GBQ ( IJDIM, KMAX       )  !! perturbation humidity at n-1

      REAL*8     WFVOR ( NMDIM, KMAX     )  !! streamfunction
      REAL*8     WFDIV ( NMDIM, KMAX     )  !! velocity potential

      REAL*8     GTU ( IJDIM, KMAX       )  !! true solution of u-wind 
      REAL*8     GTV ( IJDIM, KMAX       )  !! true solution of v-wind
      REAL*8     GTT ( IJDIM, KMAX       )  !! true solution of temp.
      REAL*8     GTPS( IJDIM             )  !! true solution of Ps
      REAL*8     GTQ ( IJDIM, KMAX       )  !! true solution of humidity

      REAL*8     UVFACT( IJDIM  )           !! factor of u -> U
      REAL*8     EDEL  ( NMDIM  )           !! vor.,D -> U,V
      REAL*8     GFTV  ( IJDIM, KMAX     )  !! virtulal tmp.
      REAL*8     GFPI( IJDIM             )  !! BS ln Ps
      REAL*8     GFALP( IJDIM            )  !! BS specific volume
      REAL*8     PS2SLP( IJDIM           )  !! factor for SL reduction

      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix

      SAVE       GBU, GBV, GBT, GBPS, GBQ
      SAVE       WFVOR, WFDIV
      SAVE       GTU, GTV, GTT, GTPS, GTQ
      SAVE       NMO, UVFACT, EDEL 
      SAVE       GFPI, GFALP, PS2SLP
*
*   [INTERNAL PARAM.] 
      CHARACTER  FTRU *(NFILN) !! file name for the true solution (in)
      CHARACTER  FERR *(NFILN) !! file name for the RMS error (output)
      REAL*8     TLAPSX         !! lapse rate
      LOGICAL    OVD            !! true solution (u,v) or (psi,chi)
      LOGICAL    OERR           !! dump error estimate or not
      LOGICAL    OFIRST

      NAMELIST  /NMERR/  OERR, OVD, FTRU, FERR

      DATA       OERR   / .FALSE. /
      DATA       OVD    / .FALSE. /
      DATA       OFIRST / .TRUE.  /
      DATA       TLAPSX / 6.5D-3  /
*
*
      IF ( OFIRST ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* ) ' @@@ DUMPRSP: DUMP STEADY RESPONSE 05/05/26'
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMERR, END=190 )
 190     WRITE  ( JFPAR, NMERR )
*
*
*
         CALL DSETNM
     O         ( NMO    )
         CALL   DSETUV
     O        ( UVFACT )
         CALL   DSETED
     O        ( EDEL   )
*
*     evaluate error from true solution
*
         IF( OERR ) THEN
            OPEN( 80, FILE = FERR, 
     &         FORM='UNFORMATTED', STATUS='UNKNOWN')
            OPEN( 81, FILE = FTRU, 
     &         FORM='UNFORMATTED', STATUS='UNKNOWN')
            CALL READGR ( GTU , 81, IDIM, JDIM, KDIM, GR4 )
            CALL READGR ( GTV , 81, IDIM, JDIM, KDIM, GR4 )
            CALL READGR ( GTT , 81, IDIM, JDIM, KDIM, GR4 )
            CALL READGR ( GTQ , 81, IDIM, JDIM, KDIM, GR4 ) !! dummy
            CALL READGR ( GTPS, 81, IDIM, JDIM, 1   , GR4 )
#ifndef OPT_CLASSIC
            CALL READGR ( GTQ , 81, IDIM, JDIM, KDIM, GR4 ) 
#else
            CALL RESET( GTQ , IJDIM*KMAX )
#endif
            CLOSE( 81 )
*
            IF( OVD ) THEN
               CALL G2W
     O              ( WDX   ,
     I                GTU   ,
     F                '    ', 'POS ', KMAX     )
               CALL G2W
     O              ( WDY   ,
     I                GTV   ,
     F                '    ', 'POS ', KMAX     )
               CALL W2G
     O              ( GTU   ,
     I                WDX   ,
     I                'YGRA', 'NEG ', KMAX     )
               CALL W2G
     M              ( GTV   ,
     I                WDX   ,
     I                'XGRA', 'POS ', KMAX     )
               CALL W2G
     M              ( GTU   ,
     I                WDY   ,
     I                'XGRA', 'ADD ', KMAX     )
               CALL W2G
     O              ( GTV   ,
     I                WDY   ,
     I                'YGRA', 'ADD ', KMAX     )
               DO 1000 K = 1, KMAX
                  DO 1000 IJ = 1, IJDIM
                     GTU( IJ,K ) = GTU( IJ,K ) / UVFACT( IJ ) / ER
                     GTV( IJ,K ) = GTV( IJ,K ) / UVFACT( IJ ) / ER
 1000          CONTINUE
            ENDIF

         ENDIF                  !! OERR
*
*     basic state
*
         CALL SETPSF( GFPS )
*
         CALL    UV2VDW         !! (U,V) -> (vor.,D)spectral
     O        ( WFVOR , WFDIV ,
     I          GFU   , GFV   , .TRUE., .TRUE.,
     W          GDX   , GDY                     )
         CALL COPY( WDX  , WFVOR, NMDIM*KMAX )
         CALL COPY( WDY  , WFDIV, NMDIM*KMAX )
         DO 1100 K = 1, KMAX
            WDX( NMO(1,0,0),K ) = 0.D0
            WDY( NMO(1,0,0),K ) = 0.D0
 1100    CONTINUE 
         DO 1200 K = 1, KMAX
            DO 1200 NM = 1, NMDIM
               WDX( NM,K ) = WDX( NM,K ) * EDEL( NM ) * ER 
               WDY( NM,K ) = WDY( NM,K ) * EDEL( NM ) * ER 
 1200    CONTINUE
         CALL W2G
     O        ( GDPSI ,
     I          WDX   ,
     I          '    ', 'POS ', KMAX )
         CALL W2G
     O        ( GDCHI ,
     I          WDY   ,
     I          '    ', 'POS ', KMAX )
*
         CALL DIAGWB
     O        ( GDW  ,
     I          GFU  , GFV  ,         GFPS   )
*
         CALL VIRTMB( GFTV , GFT , GFQ  )
         DO 1300 IJ = 1, IJDIM
            GFPI( IJ )  = LOG( GFPS( IJ ) )
            GFALP( IJ ) = ( RAIR * GFTV( IJ,1 ) ) / GFPS( IJ ) 

            TVS    = GFTV(IJ,1) * ( 1.D0 
     &               + TLAPSX*RAIR/GRAV*(SIGM(1)/SIG(1)-1.D0) )
            TV0    = TVS + GDZS(IJ)*TLAPSX
            IF ( TV0.GT.290.5D0 .AND. TVS.LE.290.5D0 ) THEN
               TLAPS = (290.5D0 - TVS) / GDZS(IJ)
            ELSEIF ( TV0.GT.290.5D0 .AND. TVS.GT.290.5D0 ) THEN
               TLAPS = 0.D0
               TVS    = 0.5D0*(290.5D0 + TVS)
            ELSEIF ( TVS.LT.255.D0 ) THEN
               TVS    = 0.5D0*(255.D0 + TVS)
            ENDIF
            TVX = TLAPS * GDZS(IJ) / TVS
            TVX = 1.D0 - TVX / 2.D0 + TVX*TVX / 3.D0
            PS2SLP( IJ ) = EXP( GRAV*GDZS(IJ)/(RAIR*TVS)*TVX )
            GDSLP( IJ ) = GFPS( IJ ) * PS2SLP( IJ )
 1300    CONTINUE 
*
         CALL RESET( GBU , IJDIM*KMAX )
         CALL RESET( GBV , IJDIM*KMAX )
         CALL RESET( GBT , IJDIM*KMAX )
         CALL RESET( GBPS, IJDIM      )
         CALL RESET( GBQ , IJDIM*KMAX )

         OFIRST = .FALSE.
*
*     save basic state
*
         CALL GHYDRO            !! hydrostatic eq.
     O        ( GDZ   ,
     I          GFTV  , GDZS  )
         CALL DUMPGRS( GDPSI, IDIM , JDIM , KMAX )
         CALL DUMPGRS( GDCHI, IDIM , JDIM , KMAX )
         CALL DUMPGRS( GFU  , IDIM , JDIM , KMAX )
         CALL DUMPGRS( GFV  , IDIM , JDIM , KMAX )
         CALL DUMPGRS( GDW  , IDIM , JDIM , KMAX )
         CALL DUMPGRS( GFT  , IDIM , JDIM , KMAX )
         CALL DUMPGRS( GDZ  , IDIM , JDIM , KMAX )
         CALL DUMPGRS( GFPS , IDIM , JDIM , 1    )
CCC         CALL DUMPGRS( GDSLP, IDIM , JDIM , 1    )
#ifndef OPT_CLASSIC
         CALL DUMPGRS( GFQ  , IDIM , JDIM , KMAX )
#endif

      ENDIF
*
*
*
      CALL COPY( GDU , GAU , IJDIM*KMAX )
      CALL COPY( GDV , GAV , IJDIM*KMAX )
      CALL COPY( GDT , GAT , IJDIM*KMAX )
      CALL COPY( GDPS, GAPI, IJDIM      )
      CALL COPY( GDQ , GAQ , IJDIM*KMAX )
*
      DO 1400 IJ = 1, IJDIM
         GDPS( IJ ) =   DEXP( GDPS( IJ ) + GFPI( IJ ) )
     &                - DEXP( GFPI( IJ ) )
 1400 CONTINUE 
*
*     dump only for initial guess and last solution
*
      IF( OINIT .OR. OLAST ) THEN
*
*     add basic state
*
         DO 5000 K = 1, KMAX
            DO 5000 IJ = 1, IJDIM
               GDU( IJ,K ) = GDU( IJ,K ) + GFU( IJ,K )
               GDV( IJ,K ) = GDV( IJ,K ) + GFV( IJ,K )
               GDT( IJ,K ) = GDT( IJ,K ) + GFT( IJ,K )
               GDQ( IJ,K ) = GDQ( IJ,K ) + GFQ( IJ,K )
 5000    CONTINUE
         DO 5100 IJ = 1, IJDIM
            GDPS( IJ ) = GDPS( IJ ) + GFPS( IJ )
 5100    CONTINUE
         DO 5200 K = 1, KMAX
            DO 5200 NM = 1, NMDIM
               WDX( NM,K ) = WDVOR( NM,K ) + WFVOR( NM,K )
               WDY( NM,K ) = WDDIV( NM,K ) + WFDIV( NM,K )
 5200    CONTINUE
         CALL SETPSF( GDPS )
*
* ======= other diagnostic variables ==============
*
*     psi/chi
*
         DO 2000 K = 1, KMAX
            WDX( NMO(1,0,0),K ) = 0.D0
            WDY( NMO(1,0,0),K ) = 0.D0
 2000    CONTINUE 
         DO 2100 K = 1, KMAX
            DO 2100 NM = 1, NMDIM
               WDX( NM,K ) = WDX( NM,K ) * EDEL( NM ) * ER 
               WDY( NM,K ) = WDY( NM,K ) * EDEL( NM ) * ER 
 2100    CONTINUE
         CALL W2G
     O        ( GDPSI ,
     I          WDX   ,
     I          '    ', 'POS ', KMAX )
         CALL W2G
     O        ( GDCHI ,
     I          WDY   ,
     I          '    ', 'POS ', KMAX )
*
*     geopotential height
*
         CALL VIRTMD
     O        ( GDTV  ,
     I          GDT   , GFQ     )
         CALL GHYDRO            !! hydrostatic eq.
     O        ( GDZ   ,
     I          GDTV  , GDZS  )
*
*     omega
*
         CALL DIAGWB
     O        ( GDW  ,
     I          GDU  , GDV  ,         GDPS   )
*
*     sea level pressure (following ECMWF)
*
         DO 2200 IJ = 1, IJDIM
            GDSLP( IJ ) = GDPS( IJ ) * PS2SLP( IJ )
 2200    CONTINUE
*
*     dump response
*
         CALL DUMPGRA( IUNITR, GDPSI, IDIM , JDIM , KMAX, 1 )
         CALL DUMPGRA( IUNITR, GDCHI, IDIM , JDIM , KMAX, 2 )
         CALL DUMPGRA( IUNITR, GDU  , IDIM , JDIM , KMAX, 3 )
         CALL DUMPGRA( IUNITR, GDV  , IDIM , JDIM , KMAX, 4 )
         CALL DUMPGRA( IUNITR, GDW  , IDIM , JDIM , KMAX, 5 )
         CALL DUMPGRA( IUNITR, GDT  , IDIM , JDIM , KMAX, 6 )
         CALL DUMPGRA( IUNITR, GDZ  , IDIM , JDIM , KMAX, 7 )
         CALL DUMPGRA( IUNITR, GDPS , IDIM , JDIM , 1   , 8 )
CCC         CALL DUMPGRA( IUNITR, GDSLP, IDIM , JDIM , 1   , 8 )
#ifndef OPT_CLASSIC
         CALL DUMPGRA( IUNITR, GDQ  , IDIM , JDIM , KMAX , 9 )
#endif
*
*     subtract basic state
*
         DO 2300 K = 1, KMAX
            DO 2300 IJ = 1, IJDIM
               GDU( IJ,K ) = GDU( IJ,K ) - GFU( IJ,K )
               GDV( IJ,K ) = GDV( IJ,K ) - GFV( IJ,K )
               GDT( IJ,K ) = GDT( IJ,K ) - GFT( IJ,K )
               GDQ( IJ,K ) = GDQ( IJ,K ) - GFQ( IJ,K )
 2300    CONTINUE
         DO 2400 IJ = 1, IJDIM
            GDPS( IJ ) = GDPS( IJ ) - GFPS( IJ )
 2400    CONTINUE

      ENDIF
*
*     Norm difference from previous iteration
*
      DNORM = 0.D0
      DO 3000 K = 1, KMAX
         DO 3100 IJ = 1, IJDIM
            DNORM = DNORM 
     &           + ( GDU( IJ,K ) - GBU( IJ,K ) )**2 / 2.
     &           + ( GDV( IJ,K ) - GBV( IJ,K ) )**2 / 2.
     &           + CP * DABS( ( GDT( IJ,K ) - GBT( IJ,K ) ) )
#ifndef OPT_CLASSIC
     &           + EL * DABS( ( GDQ( IJ,K ) - GBQ( IJ,K ) ) )
#endif
 3100    CONTINUE
 3000 CONTINUE
      DO 3200 IJ = 1, IJDIM
         DNORM = DNORM 
     &           + GFALP( IJ ) * DABS( ( GDPS( IJ ) - GBPS( IJ ) ) )
 3200 CONTINUE
*
*     RMS error and dump to file (if specified)
*
      IF( OERR ) THEN
         RMSE = 0.D0
         DO 4000 K = 1, KMAX
            DO 4100 IJ = 1, IJDIM
               RMSE = RMSE 
     &              + ( GDU( IJ,K ) - GTU( IJ,K ) )**2 / 2.
     &              + ( GDV( IJ,K ) - GTV( IJ,K ) )**2 / 2.
     &              + CP * DABS( ( GDT( IJ,K ) - GTT( IJ,K ) ) )
#ifndef OPT_CLASSIC
     &              + EL * DABS( ( GDQ( IJ,K ) - GTQ( IJ,K ) ) )
#endif
 4100       CONTINUE
 4000    CONTINUE
         DO 4200 IJ = 1, IJDIM
            RMSE = RMSE 
     &           + GFALP( IJ ) * DABS( ( GDPS( IJ ) - GTPS( IJ ) ) )
 4200    CONTINUE
*
         IF( .NOT. OLAST )
     &        WRITE( 80 ) SNGL( RMSE ), SNGL( DNORM )
      ENDIF
*
*     store values
*
      CALL COPY( GBU , GDU , IJDIM*KMAX )
      CALL COPY( GBV , GDV , IJDIM*KMAX )
      CALL COPY( GBT , GDT , IJDIM*KMAX )
      CALL COPY( GBPS, GDPS, IJDIM      )
      CALL COPY( GBQ , GDQ , IJDIM*KMAX )
*
      RETURN
*========================================================
      ENTRY CLR_DUMPRSP          !! clear buffer
*
      CALL RESET( GBU , IJDIM*KMAX )
      CALL RESET( GBV , IJDIM*KMAX )
      CALL RESET( GBT , IJDIM*KMAX )
      CALL RESET( GBPS, IJDIM      )
      CALL RESET( GBQ , IJDIM*KMAX )
*
      RETURN
      END
**********************************************************************
      SUBROUTINE DUMPGRA    !! dump data to R4 GrADS format
     I         ( IUNT, GDX, IDIMD, JDIMD, KDIMD, IVAR )
*
*     dump output
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */

      INTEGER   MAXLEV
      INTEGER   MAXVAR
      PARAMETER ( MAXLEV = 50 )
      PARAMETER ( MAXVAR = 50 )
*
*   [INPUT]
      INTEGER   IUNT
      INTEGER   IDIMD, JDIMD, KDIMD
      INTEGER   IVAR
      REAL*8    GDX( IDIMD,JDIMD,KDIMD )
*
*   [WORK]
      REAL*4    GDW( IDIMD-1,JDIMD )
      REAL*8    GPW( IDIM,JDIM,MAXLEV )
      INTEGER   NPLEV
      INTEGER   IFPAR, JFPAR
      INTEGER   I, J, K
*
*   [SAVE]
      REAL*8    GPWS( IDIM,JDIM,MAXLEV,MAXVAR )
      INTEGER   JVAR

      SAVE      GPWS
      SAVE      JVAR
*
*   [PARAMETER]
      LOGICAL    OFIRST
      LOGICAL    OS2P
      LOGICAL    OANM
      REAL*8     XMISS
      SAVE       OS2P
      SAVE       OANM

      NAMELIST  /NMS2P/ OS2P
      NAMELIST  /NMANM/ OANM

      DATA       OFIRST / .TRUE.  /
      DATA       OS2P   / .FALSE. /
      DATA       OANM   / .TRUE.  /
      DATA       XMISS  / -999.D0 /

      IF( OFIRST ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* ) ' ### DUMPGRA: DUMPGRS MUST BE CALLED FIRST'
         CALL XABORT( 1 )
         RETURN
      ENDIF

      IF( OS2P .AND. KDIMD .GT. 1 ) THEN
         CALL S2PINT
     O        ( GPW   , NPLEV ,
     I          GDX   , KDIMD , 'ALEV', XMISS ,
     O          1 ,     1 ,  IDIMD, JDIMD   )
         DO 1000 K = 1, NPLEV
            DO 1100 J = 1, JDIMD
               DO 1100 I = 1, IDIMD
                  IF( GPW( I,J,K )       .EQ. XMISS .OR.
     &                GPWS( I,J,K,IVAR ) .EQ. XMISS     ) THEN
                     GPW( I,J,K ) = XMISS
                  ELSE
                     GPW( I,J,K ) = 
     &                    GPW( I,J,K ) - GPWS( I,J,K,IVAR )
                  ENDIF
 1100       CONTINUE
            WRITE( IUNT )
     &           (( SNGL( GPW(I,J,K) ),I=1,IDIMD-1),J=1,JDIMD)

 1000    CONTINUE
      ELSE
         DO 1200 K = 1, KDIMD
            WRITE( IUNT ) 
     &           (( SNGL( GDX(I,J,K)-GPWS( I,J,K,IVAR ) ),
     &                    I=1,IDIMD-1),J=1,JDIMD) 
 1200    CONTINUE
      ENDIF
*
      RETURN
*========================================================
      ENTRY DUMPGRS         !! save basic state
     I         ( GDX, IDIMD, JDIMD, KDIMD )

      IF( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMS2P, END=190 )
 190     WRITE  ( JFPAR, NMS2P )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMANM, END=290 )
 290     WRITE  ( JFPAR, NMANM )
         WRITE ( JFPAR,* ) ' @@@ DUMPGRA: DUMP RESPONSE 05/07/21'
         IF( OS2P ) WRITE ( JFPAR,* ) '    @@@ S->P: '
         JVAR = 0

         CALL RESET( GPWS , IDIM*JDIM*MAXLEV*MAXVAR )
      ENDIF

      JVAR = JVAR + 1

      IF( .NOT. OANM ) RETURN

      IF( KDIMD .GT. 1) THEN 
         IF( OS2P ) THEN
            CALL S2PINT
     O           ( GPW   , NPLEV ,
     I             GDX   , KDIMD , 'ALEV', XMISS ,
     O                 1 ,     1 ,  IDIMD, JDIMD   )
            DO 2000 K = 1, NPLEV
               DO 2000 J = 1, JDIMD
                  DO 2000 I = 1, IDIMD
                  GPWS( I,J,K,JVAR ) = GPW( I,J,K )
 2000       CONTINUE
         ELSE
            DO 2100 K = 1, KDIMD
               DO 2100 J = 1, JDIMD
                  DO 2100 I = 1, IDIMD
                     GPWS( I,J,K,JVAR ) = GDX( I,J,K )
 2100       CONTINUE
         ENDIF
      ELSE
         DO 2200 J = 1, JDIMD
            DO 2200 I = 1, IDIMD
            GPWS( I,J,1,JVAR ) = GDX( I,J,1 )
 2200    CONTINUE
      ENDIF

      RETURN
*========================================================
      ENTRY READGR         !! read R4 GrADS format data
     O         ( GDX, 
     I           IUNT, IDIMD, JDIMD, KDIMD,
     W           GDW  )
*
*
      DO 3000 K = 1, KDIMD
         READ( IUNT ) GDW
         DO 3100 J = 1, JDIMD
            DO 3200 I = 1, IDIMD-1
               GDX( I,J,K ) = DBLE( GDW( I,J ) )
 3200       CONTINUE
            GDX( IDIMD,J,K ) = DBLE( GDW( 1,J ) )
 3100    CONTINUE
 3000 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE RDSFRC         !!  get steady forcing
     M         ( XP    ,
     I           MAXN  , IUNITF, NSFCS , DELT  , WPHIS , 
     C           FLAPLA, GG    , TMTX  , WHMTX , GCMTX  )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY]
      REAL*8     XP( MAXN,0:MMAX )
*
*   [INPUT]
      INTEGER    MAXN
      INTEGER    IUNITF
      INTEGER    NSFCS
      REAL*8     DELT                        !! time step delta(t)
      REAL*8     WPHIS ( NMDIM  )         !! surface G.P.

      REAL*8     GG    ( KMAX )           !! for semi-implicit
      REAL*8     TMTX  ( KMAX, KMAX )     !! for semi-implicit
      REAL*8     WHMTX ( KMAX, KMAX )     !! for semi-implicit
      REAL*8     GCMTX ( KMAX, KMAX )     !! for semi-implicit
      REAL*8     FLAPLA( NMDIM )          !! Laplacian factor
*
*   [WORK]
      REAL*8     GSFV ( IJDIM, KMAX )    !! steady forcing for vor.
      REAL*8     GSFD ( IJDIM, KMAX )    !! steady forcing for div.
      REAL*8     GSFT ( IJDIM, KMAX )    !! steady forcing for temp.
      REAL*8     GSFP ( IJDIM       )    !! steady forcing for LnPs

      REAL*8     WSVOR ( NMDIM, KMAX   )  !! vorticity
      REAL*8     WSDIV ( NMDIM, KMAX   )  !! divergence D
      REAL*8     WST   ( NMDIM, KMAX   )  !! temperature
      REAL*8     WSPI  ( NMDIM         )  !! logPs pi
      REAL*8     WSQ   ( NMDIM, KMAX   )  !! humidity
*
      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDPHIX( NMDIM  )         !! work
*
      REAL*4     X     ( IMAX, JMAX    ) !! temp array for forcing data 

      INTEGER    K, IJ, I, J, L, M, IW, LEND, NM
      INTEGER    IFPAR, JFPAR
*
*   [SAVE]
      REAL*8     WXVOR( 2*NMAX, KMAX, 0:MMAX )
      REAL*8     WXDIV( 2*NMAX, KMAX, 0:MMAX )
      REAL*8     WXTMP( 2*NMAX, KMAX, 0:MMAX )
      REAL*8     WXPI ( 2*NMAX,       0:MMAX )
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER    JW( 0:MMAX )
      INTEGER    NSFCSX

      SAVE       WXVOR, WXDIV, WXTMP, WXPI
      SAVE       NMO
      SAVE       JW
      SAVE       NSFCSX
*
*   [PARAM]
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
      DATA       NSFCSX / -999   /
*
*
      IF( OFIRST ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* ) ' @@@ RDSFRC: READ STEADY FORCING 05/06/21'
         CALL DSETNM
     O        ( NMO    )
*
         DO 500 M = 0, MMAX
            LEND = MIN( LMAX, NMAX-M)
            JW( M ) = 0
            DO 510 K = 1, KMAX
               IW = 0
               DO 520 L = 0, LEND
                  IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 520
                  IW = IW + 1
                  IF( M .EQ. 0 ) GOTO 520
                  IW = IW + 1
 520           CONTINUE
 510        CONTINUE
            JW( M ) = IW
 500     CONTINUE
*
         CALL RESET( WXVOR , 2*NMAX*KMAX*( MMAX+1 ) )
         CALL RESET( WXDIV , 2*NMAX*KMAX*( MMAX+1 ) )
         CALL RESET( WXTMP , 2*NMAX*KMAX*( MMAX+1 ) )
         CALL RESET( WXPI  ,      2*NMAX*( MMAX+1 ) )
*
         OFIRST = .FALSE.
      ENDIF


      IF( IUNITF .LT. 0 ) NSFCSX = NSFCS !! skip reading forcing

      IF( NSFCS .NE. NSFCSX ) THEN
         CALL FREWND( IUNITF )
         NSFCSX = NSFCS
*
*     read forcing data
*
         K = ( NSFCSX-1 ) * ( 3*KMAX+1 )
         IF( K .GT. 0 ) THEN
            DO 90 I = 1, K
               READ( IUNITF ) X !! dummy
 90         CONTINUE
         ENDIF
*     
         DO 100 K = 1, KMAX
            READ( IUNITF ) X    !! vorticity
            IJ = 0
            DO 110 J = 1, JMAX
               DO 120 I = 1, IMAX
                  IJ = IJ + 1
                  GSFV( IJ, K) = DBLE( X( I, J) )
 120           CONTINUE
               IJ = IJ + 1
               GSFV( IJ, K) = DBLE( X( 1, J) )
 110        CONTINUE
 100     CONTINUE
         write(6,*) 'vor.'
*     
         DO 130 K = 1, KMAX
            READ( IUNITF ) X    !! divergence
            IJ = 0
            DO 140 J = 1, JMAX
               DO 150 I = 1, IMAX
                  IJ = IJ + 1
                  GSFD( IJ, K) = DBLE( X( I, J) )
 150           CONTINUE
               IJ = IJ + 1
               GSFD( IJ, K) = DBLE( X( 1, J) )
 140        CONTINUE
 130     CONTINUE
         write(6,*) 'div.'
*     
         DO 160 K = 1, KMAX
            READ( IUNITF ) X    !! temperature
            IJ = 0
            DO 170 J = 1, JMAX
               DO 180 I = 1, IMAX
                  IJ = IJ + 1
                  GSFT( IJ, K) = DBLE( X( I, J) )
 180           CONTINUE
               IJ = IJ + 1
               GSFT( IJ, K) = DBLE( X( 1, J) )
 170        CONTINUE
 160     CONTINUE
         write(6,*) 'tmp.'
*     
         READ( IUNITF ) X       !! Ps
         IJ = 0
         DO 190 J = 1, JMAX
            DO 200 I = 1, IMAX
               IJ = IJ + 1
               GSFP( IJ   ) = DBLE( X( I, J) )
 200        CONTINUE
            IJ = IJ + 1
            GSFP( IJ   ) = DBLE( X( 1, J) )
 190     CONTINUE
         write(6,*) 'lnps'
*     
*     grid to wave
*
         CALL G2W
     O        ( WSVOR ,
     I        GSFV  ,
     F        '    ', 'POS ', KMAX     )
         CALL G2W
     O        ( WSDIV ,
     I        GSFD  ,
     F        '    ', 'POS ', KMAX     )
         CALL G2W
     O        ( WST   ,
     I        GSFT  ,
     F        '    ', 'POS ', KMAX     )
         CALL G2W
     O        ( WSPI  ,
     I        GSFP  ,
     F        '    ', 'POS ', 1        )
*
*     semi-implicit treatment for D,T,pi forcing
*
         CALL MODFRC
     M         ( WSDIV , WST   , WSPI  , 
     I           DELT  , WPHIS , 
     C           FLAPLA, GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDPHIX                         )
*
*     store in vectors
*
         DO 300 M = 0, MMAX
            LEND = MIN( LMAX, NMAX-M)
            DO 310 K = 1, KMAX
               IW = 0
               DO 320 L = 0, LEND
                  IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 320
                  I = NMO( 1, M, L)
                  J = NMO( 2, M, L)
                  IW = IW + 1
                  WXVOR(IW,K,M) = WSVOR(I,K)
                  WXDIV(IW,K,M) = WSDIV(I,K)
                  WXTMP(IW,K,M) = WST  (I,K)
                  WXPI (IW  ,M) = WSPI (I  )
                  IF( M .EQ. 0 ) GOTO 320
                  IW = IW + 1
                  WXVOR(IW,K,M) = WSVOR(J,K)
                  WXDIV(IW,K,M) = WSDIV(J,K)
                  WXTMP(IW,K,M) = WST  (J,K)
                  WXPI (IW  ,M) = WSPI (J  )
 320           CONTINUE
 310        CONTINUE
 300     CONTINUE
*
      ENDIF
*
*     add forcing to (La+R)X
*
      DO 490 M = 0, MMAX
         NM = 0
         DO 400 K = 1, KMAX
            DO 400 I = 1, JW( M )
               NM = NM + 1
               XP( NM,M ) = XP( NM,M ) + WXVOR( I,K,M )
 400     CONTINUE
         DO 410 K = 1, KMAX
            DO 410 I = 1, JW( M )
               NM = NM + 1
               XP( NM,M ) = XP( NM,M ) + WXDIV( I,K,M )
 410     CONTINUE
         DO 420 K = 1, KMAX
            DO 420 I = 1, JW( M )
               NM = NM + 1
               XP( NM,M ) = XP( NM,M ) + WXTMP( I,K,M )
 420     CONTINUE
         DO 430 I = 1, JW( M )
            NM = NM + 1
            XP( NM,M ) = XP( NM,M ) + WXPI( I,M )
 430     CONTINUE
 490  CONTINUE
*
*
      RETURN
      END
