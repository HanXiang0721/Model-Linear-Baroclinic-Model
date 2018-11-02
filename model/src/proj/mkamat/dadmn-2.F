* PACKAGE DADMN !! dynamics: main routine
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/03(numaguti) tracer
*         96/06/06(numaguti) remove FIXMAS/MASFIX
*         01/06/15(hiro)     PWM incl. moisture
**********************************************************************
      SUBROUTINE DYNMCS    !!  dynamics
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GFPSM ,
     O           GSIGD , GPID  ,
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
      REAL*8     GSIGD ( IJDIM, KMAX+1    )  !! sigma velocity
      REAL*8     GPID  ( IJDIM            )  !! pi velocity
*
*   [INPUT]
      REAL*8     GFU   ( IJDIM, KMAX      )  !! BS westerly u 
      REAL*8     GFV   ( IJDIM, KMAX      )  !! BS southern wind v
      REAL*8     GFT   ( IJDIM, KMAX      )  !! BS temperature
      REAL*8     GFPS  ( IJDIM            )  !! BS surface pressure
      REAL*8     GFPSM ( IJDIM            )  !! ZMBS surface pressure
      REAL*8     GFQ   ( IJDIM, KMAX      )  !! BS humidity etc.

      REAL*8     TIME                        !! time
      REAL*8     DELT                        !! time step delta(t)
      REAL*8     GDZS  ( IJDIM )             !! surface topography
*
*   [INTERNAL]
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GAPS  ( IJDIM            )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX      )  !! humidity etc.
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX      )  !! humidity etc.
      SAVE       GBU, GBV, GBT, GBPS, GBQ
*        : grid value(t) perturbation
*
*   [INTERNAL WORK] 
      REAL*8     GAVOR ( IJDIM, KMAX   )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX   )  !! divergence
      REAL*8     GATV  ( IJDIM, KMAX   )  !! virtual temperature
      REAL*8     GDX   ( IJDIM, KMAX+1 )  !! work
      REAL*8     GDY   ( IJDIM, KMAX   )  !! work
      REAL*8     GDZ   ( IJDIM, KMAX   )  !! work

      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work
      REAL*8     WTVOR ( NMDIM, KMAX   )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX   )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX   )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM         )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX   )  !! tendency of humidity
*
      REAL*8     WXVOR ( NMDIM, KMAX, 0:MMAX ) !! work
      REAL*8     WXDIV ( NMDIM, KMAX, 0:MMAX ) !! work
      REAL*8     WXTMP ( NMDIM, KMAX, 0:MMAX ) !! work
      REAL*8     WXPI  ( NMDIM      , 0:MMAX ) !! work
      REAL*8     WXSPH ( NMDIM, KMAX, 0:MMAX ) !! work
*
      INTEGER    IFPAR, JFPAR
      INTEGER    NM
      INTEGER    NX
      INTEGER    NXW( 0:MMAX )
      INTEGER    IW, IS
      INTEGER    M, L, IC, LEND, LEND0, LSTR, LL, K, MM
      SAVE       M, LEND, LEND0
*
*   [INTERNAL SAVE] 
      REAL*8     DIFV  ( NMDIM, KMAX   )  !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX   )  !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX   )  !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX   )  !! h-diff. coef. of water
      REAL*8     WPHIS ( NMDIM  )         !! surface G.P.
      REAL*8     WTUNTT( NMDIM, KMAX   )  !! unit matrix
      REAL*8     WTUNTF( NMDIM, KMAX   )  !! zero matrix (dumy) 
      REAL*8     WTUNTP( NMDIM         )  !! unit matrix 
*
      REAL*8     SDTOP ( IJDIM         ) !! coefficient for diss.
      REAL*8     SDBTM ( IJDIM         ) !! coefficient for diss.
*
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER    NOMWV ( NMDIM )
*
      SAVE       DIFV, DIFD, DIFT, DIFQ, WPHIS
      SAVE       SDTOP, SDBTM
      SAVE       NMO, NOMWV
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
      SAVE       SALPHA, SBETA, SKAPPA, ATF, BTF, TBAR
      SAVE       GG, TMTX, WHMTX, GCMTX
      SAVE       TDMPC
*
      REAL*8     CORIOL( IJDIM )          !! Coriolis factor f
      REAL*8     FLAPLA( NMDIM )          !! Laplacian factor
      SAVE       CORIOL, FLAPLA
*     
*   [INTERNAL PARM] 
      CHARACTER  AMATF *(NFILN) !! file name for the matrix column
      LOGICAL    OMATV          !! add unity to the vorticity?
      LOGICAL    OMATD          !! add unity to the divergence?
      LOGICAL    OMATT          !! add unity to the temperature?
      LOGICAL    OMATP          !! add unity to the sfc. pressure?
      LOGICAL    OMATQ          !! add unity to the temperature?
      INTEGER    KMATW          !! position of adding unity in wave
      INTEGER    KMATL          !! position of adding unity in sigma
      NAMELIST  /NMAMAT/ AMATF, 
     $           OMATV, OMATD, OMATT, OMATP, OMATQ, KMATW, KMATL
      SAVE       OMATV, OMATD, OMATT, OMATP, OMATQ
      SAVE       KMATW, KMATL
      LOGICAL    OFIRST
      DATA       AMATF  / '     ' /
      DATA       OFIRST / .TRUE.  /
      LOGICAL    OVWRT
      DATA       OVWRT  / .TRUE.  /
      DATA       OMATV  / .FALSE. /
      DATA       OMATD  / .FALSE. /
      DATA       OMATT  / .FALSE. /
      DATA       OMATP  / .FALSE. /
      DATA       OMATQ  / .FALSE. /
      DATA       KMATW  / 0       /
      DATA       KMATL  / 0       /
*
      REAL*8     UNTV           !! unity for vorticity
      DATA       UNTV   / 1.0D-7 /
      REAL*8     UNTD           !! unity for divergence
      DATA       UNTD   / 1.0D-7 /
      REAL*8     UNTT           !! unity for temperature
      DATA       UNTT   / 1.0D-3 /
      REAL*8     UNTP           !! unity for log(Ps)
      DATA       UNTP   / 1.0D-5 /
      REAL*8     UNTQ           !! unity for humidity
      DATA       UNTQ   / 1.0D-6 /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADMN: DYNAMICS CONTROL 98/10/30'
         OFIRST = .FALSE.
*
         CALL DSETNM
     O         ( NMO    )
         CALL SPSTUP      !! spherical harmonic functions
         CALL G2W
     O         ( WPHIS ,
     I           GDZS  ,
     F           '    ', 'POS ', 1     )
         DO 200 NM = 1, NMDIM
***            WPHIS( NM ) = WPHIS( NM )*GRAV
            WPHIS( NM ) = 0.D0
            NOMWV( NM ) = 1
  200    CONTINUE 
*
         CALL DSETDF      !! horizontal diffusion
     O          ( DIFV  , DIFD  , DIFT  , DIFQ  ,
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
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMAMAT, END=290 )
  290    WRITE  ( JFPAR, NMAMAT )
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
#ifdef OPT_MKMAT
*
*     loop for l
*
      L = -1
  111 LEND = LEND0
      L = L + 1
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
      CALL RESET( WTUNTF, NMDIM*KMAX )
      CALL RESET( WTUNTP, NMDIM      )
*
      IF( OMATV ) WTUNTT( NMO(IC,M,L), KMATL) = UNTV
      IF( OMATD ) WTUNTT( NMO(IC,M,L), KMATL) = UNTD
      IF( OMATT ) WTUNTT( NMO(IC,M,L), KMATL) = UNTT
      IF( OMATP ) WTUNTP( NMO(IC,M,L)       ) = UNTP
      IF( OMATQ ) WTUNTT( NMO(IC,M,L), KMATL) = UNTQ
#endif
*
*  << tendency term >>
*
      CALL DYNTRM    !! dynamics terms
     O         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     O           GSIGD , GPID  , GAVOR , GADIV , GATV  , 
     O           GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     M           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           DELT  ,
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   , 
     I           GFPSM , SDTOP , SDBTM ,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA,
     W           GDX   , GDY   , GDZ   , WDX   ,
     I           OMATV , OMATD , OMATT , OMATP , OMATQ ,
     I           WTUNTT, WTUNTF, WTUNTP )
*
*  << time integration >>
*
      CALL DYNSTP
     M         ( GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     M           WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           DELT  , 
     I           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA,
     I           OMATV , OMATD , OMATT , OMATP , OMATQ ,
     I           TDMPC ,
     C           WPHIS , GG    , TMTX  , WHMTX , GCMTX , NOMWV ,
     W           GDX   , GDY   , WDX   , WDY            )
*
*     write wave comp. with unity
*
      MM = M
#ifdef OPT_WVFRC
      DO 500 MM = 0, MIN( M,MMAX )
         LEND = MIN( LMAX, NMAX-MM)
#else
#ifdef OPT_OWALL
      DO 500 MM = 0, MMAX
         LEND = MIN( LMAX, NMAX-MM)
#endif      
#endif      
      WRITE( 6, *) ' @@@ WRITE DOWN MATRIX FILE : m=',
     &     mm,' l=',l,'ic=',ic,'@@@'

      DO 300 K = 1, KMAX
         NX = 0

         LSTR = 0
         IF( MM .EQ. 0 ) LSTR = 1
         DO 310 LL = LSTR, LEND
            NX = NX + 1
            WXVOR( NX, K, MM) = WTVOR( NMO(1,MM,LL), K)
            WXDIV( NX, K, MM) = WTDIV( NMO(1,MM,LL), K)
            WXTMP( NX, K, MM) = WTT( NMO(1,MM,LL), K)
            WXSPH( NX, K, MM) = WTQ( NMO(1,MM,LL), K)
            IF( MM .NE. 0 ) THEN
               NX = NX + 1
               WXVOR( NX, K, MM) = WTVOR( NMO(2,MM,LL), K)
               WXDIV( NX, K, MM) = WTDIV( NMO(2,MM,LL), K)
               WXTMP( NX, K, MM) = WTT( NMO(2,MM,LL), K)
               WXSPH( NX, K, MM) = WTQ( NMO(2,MM,LL), K)
            ENDIF
  310    CONTINUE
  300 CONTINUE
      NX = 0
      DO 320 LL = LSTR, LEND
         NX = NX + 1
         WXPI( NX, MM ) = WTPI( NMO(1,MM,LL) )
         IF( MM .NE. 0 ) THEN
            NX = NX + 1
            WXPI( NX, MM ) = WTPI( NMO(2,MM,LL) )
         ENDIF
  320 CONTINUE
#ifndef OPT_WVFRC
#ifdef OPT_CLASSIC
      WRITE( 77 ) ((WXVOR(IW,IS,MM),IW=1,NX),IS=1,KMAX),
     $            ((WXDIV(IW,IS,MM),IW=1,NX),IS=1,KMAX),
     $            ((WXTMP(IW,IS,MM),IW=1,NX),IS=1,KMAX),
     $            ( WXPI(IW,MM)    ,IW=1,NX)
#else
      WRITE( 77 ) ((WXVOR(IW,IS,MM),IW=1,NX),IS=1,KMAX),
     $            ((WXDIV(IW,IS,MM),IW=1,NX),IS=1,KMAX),
     $            ((WXTMP(IW,IS,MM),IW=1,NX),IS=1,KMAX),
     $            ( WXPI(IW,MM)    ,IW=1,NX),
     $            ((WXSPH(IW,IS,MM),IW=1,NX),IS=1,KMAX)
#endif
#else
      NXW( MM ) = NX
#endif
  500 CONTINUE
#ifdef OPT_WVFRC
#ifdef OPT_CLASSIC
#ifdef OPT_OWALL
      WRITE( 77 ) 
     $     (((WXVOR(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $      ((WXDIV(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $      ((WXTMP(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $      ( WXPI(IW,MM)    ,IW=1,NXW(MM)),MM=0,M)
#else
      DO 550 MM = 0, MIN( M,MMAX )
         WRITE( 77 ) 
     $        ((WXVOR(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $        ((WXDIV(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $        ((WXTMP(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $        ( WXPI(IW,MM)    ,IW=1,NXW(MM))
 550  CONTINUE
#endif
#else
#ifdef OPT_OWALL
      WRITE( 77 ) 
     $     (((WXVOR(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $      ((WXDIV(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $      ((WXTMP(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $      ( WXPI(IW,MM)    ,IW=1,NXW(MM)),
     $      ((WXSPH(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $        MM=0,M)
#else
      DO 550 MM = 0, MIN( M,MMAX )
         WRITE( 77 ) 
     $        ((WXVOR(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $        ((WXDIV(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $        ((WXTMP(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX),
     $        ( WXPI(IW,MM)    ,IW=1,NXW(MM)),
     $        ((WXSPH(IW,IS,MM),IW=1,NXW(MM)),IS=1,KMAX)
 550  CONTINUE
#endif
#endif
      IF( M .LT. MMAX ) THEN 
         WRITE( 6, *) ' @@@ ZONAL WAVE TRUNCATED '
      ENDIF
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
