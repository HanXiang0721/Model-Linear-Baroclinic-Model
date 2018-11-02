* PACKAGE DSTEP !! dynamics: integration of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         00/04/10(hiro)     planetary wave model
*         01/06/15(hiro)     PWM incl. moisture
**********************************************************************
      SUBROUTINE DYNSTP    !! time step of dynamics
     M         ( GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     M           WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           DELT  , 
     I           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA, 
     I           TDMPC ,
     I           WPHIS , GG    , TMTX  , WHMTX , GCMTX , 
     W           GDX   , GDY   , WDX   , WDY            )
*     
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"                /* # of char. in string   */
#include        "zccom.F"                /* physics etc. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY] 
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX      )  !! humidity 
*
*   [INPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX      )  !! tendency of humidity
*
      REAL*8     DELT                        !! time step
      REAL*8     TDMPC          !! damping coefficient for temperature
*
      REAL*8     DIFV  ( NMDIM, KMAX )       !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX )       !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX )       !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX )       !! h-diff. coef. of tracer
      REAL*8     FLAPLA( NMDIM )             !! laplacian factor
*
      REAL*8     WPHIS ( NMDIM       )       !! topography
      REAL*8     GG    ( KMAX )              !! for semi-implicit
      REAL*8     TMTX  ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     WHMTX ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     GCMTX ( KMAX, KMAX )        !! for semi-implicit
*
*   [WORK] 
      REAL*8     GDX   ( IJDIM, KMAX )
      REAL*8     GDY   ( IJDIM, KMAX )
      REAL*8     GDZ   ( IJDIM, KMAX )
      REAL*8     GDW   ( IJDIM )
      REAL*8     WDX   ( NMDIM, KMAX )
      REAL*8     WDY   ( NMDIM, KMAX )
*
*   [INTERNAL WORK] 
      REAL*8     WDVOR ( NMDIM, KMAX )       !! vorticity
      REAL*8     WDDIV ( NMDIM, KMAX )       !! divergence D
      REAL*8     WDT   ( NMDIM, KMAX )       !! temperature T
      REAL*8     WDPI  ( NMDIM       )       !! log Ps=pi
      REAL*8     WDQ   ( NMDIM, KMAX )       !! humidity Q
*
      INTEGER    IJ, IFPAR, JFPAR

      CHARACTER  AMATF *(NFILN) !! file name for the matrix column
      NAMELIST  /NMAMAT/ AMATF
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST          / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSTEP: MODIFIED 05/06/20'
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMAMAT, END=290 )
  290    WRITE  ( JFPAR, NMAMAT )
         OFIRST = .FALSE.
      ENDIF
*
*  << G2W : grid->spectral >>
*
      CALL RESET( WDVOR , NMDIM*KMAX )
      CALL RESET( WDDIV , NMDIM*KMAX )
      CALL RESET( WDT   , NMDIM*KMAX )
      CALL RESET( WDPI  , NMDIM      )
      CALL RESET( WDQ   , NMDIM*KMAX )
      CALL RESET( WTVOR , NMDIM*KMAX )
      CALL RESET( WTDIV , NMDIM*KMAX )
      CALL RESET( WTT   , NMDIM*KMAX )
      CALL RESET( WTPI  , NMDIM      )
      CALL RESET( WTQ   , NMDIM*KMAX )
*
*  << INTGR : time integration >>
*
      CALL TINTGR                      !! spectral time integration
     M         ( WDVOR , WDDIV , WDT   , WDPI  , WDQ   ,
     M           WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           DELT  , WPHIS , 
     I           TDMPC ,
     C           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA,
     C           GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDY                            )
*
      CALL W2G 
     O     ( GDX   ,
     I       WTVOR ,
     I      '     ', 'POSO', KMAX )
      CALL W2G 
     O     ( GDY   ,
     I       WTDIV ,
     I      '     ', 'POSO', KMAX )
      CALL W2G 
     O     ( GDZ   ,
     I       WTT   ,
     I      '     ', 'POSO', KMAX )
      CALL W2G 
     O     ( GDW   ,
     I       WTPI  ,
     I      '     ', 'POSO', 1    )
      CALL WGRD
     I     ( GDX , GDY , GDZ , GDW ,
     I       AMATF , IDIM , JDIM , KMAX  )
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SPCTLD    !!  diagnosis in spectral
     I         ( WDATA , HTTL  , KDIMD )
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
*
*   [INPUT]
      INTEGER    KDIMD
      REAL*8     WDATA ( NMDIM, KDIMD )
      CHARACTER  HTTL  *(*)
*
*   [INTERNAL WORK]
      REAL*8     WAMP  ( 0:NMAX, KMAX+1 )
      LOGICAL    ODIAG
      INTEGER    JFLDIA, NCOLS, ISTR, IEND, IINT
      INTEGER    K,L,M,N
*
*   [INTERNAL SAVE] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      SAVE       NMO
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL DSETNM
     O         ( NMO    )
      ENDIF
*
      CALL QDIAGP
     O     ( ODIAG , JFLDIA, NCOLS,
     O       ISTR  , IEND  , IINT ,
     I       KDIMD                  )
*
      IF ( .NOT. ODIAG )       RETURN
*
      CALL RESET ( WAMP, (NMAX+1)*KDIMD )
*
      DO 2100 K = 1, KDIMD
         DO 2110 N = 0 , NMAX
            DO 2120 M = 0 , MMAX, MINT
               L = N - M
               IF ( ( L .GE. 0 ).AND.( L .LE. LMAX ) ) THEN
                  WAMP ( N,K ) = WAMP ( N,K )
     &                         + WDATA( NMO(1,M,L),K ) ** 2
     &                         + WDATA( NMO(2,M,L),K ) ** 2
               ENDIF
 2120      CONTINUE
 2110   CONTINUE
 2100 CONTINUE
*
      WRITE ( JFLDIA, * ) '@@@@@@@@@@@@< SPECTRAL AMPLITUDE ', HTTL,
     &                    '>@@@@@@@@@@@@@'
*
      DO 3100 K = 1, KDIMD
         CALL PRINTD
     I       ( JFLDIA, '(1PE12.4)', WAMP  ,
     I         1     , NMAX+1 , 1     , K    ,
     I         NMAX+1, KDIMD  , NCOLS         )
 3100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE WSTD         !!  draw data on a map
     I     ( XDAT ,  CVAR , IDIMD , JDIMD , KDIMD , FACT  )
*
*   [INPUT] 
      INTEGER    IDIMD, JDIMD, KDIMD
      REAL*8     FACT
      REAL*8     XDAT ( IDIMD,JDIMD,KDIMD   ) !! data
      REAL*8     YDAT ( IDIMD,JDIMD,KDIMD   ) !! data
      REAL*8     ZDAT ( IDIMD,JDIMD,KDIMD   ) !! data
      REAL*8     WDAT ( IDIMD,JDIMD         ) !! data
      REAL*8     ZM
      CHARACTER  CVAR*4
      CHARACTER  CFIL*90
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K
*
      WRITE ( 6,* ) ' *** STDOUT FOR: ', CVAR
      DO 1000 J = 1, JDIMD
         WRITE( 6,'(130I1)' ) 
     &        ( INT( XDAT( I,J,1 ) * FACT ), I = 1, IDIMD-1 )
 1000 CONTINUE
*
      RETURN
*===================================================================
      ENTRY WGRD         !!  write down data to GrADS file
     I     ( XDAT , YDAT , ZDAT , WDAT ,
     I       CFIL , IDIMD , JDIMD , KDIMD )
*
*
      WRITE ( 6,* ) ' *** GRDOUT *** ', CFIL
      WRITE ( 6,* ) IDIMD,JDIMD,KDIMD

      OPEN( 91 , FILE=CFIL , FORM='UNFORMATTED' )
      DO 2000 K = 1, KDIMD
         DO J = 1, JDIMD
            ZM = 0.D0
            DO I = 1, IDIMD-1
               ZM = ZM + XDAT( I,J,K )/FLOAT( IDIMD-1 )
            ENDDO
            DO I = 1, IDIMD-1
               XDAT( I,J,K ) = XDAT( I,J,K ) - ZM
            ENDDO
         ENDDO
         write(6,* ) k
         WRITE( 91 )
     &     ( ( SNGL( XDAT(I,J,K) ) , I = 1, IDIMD-1 ), J = 1, JDIMD ) 
 2000 CONTINUE
      DO 2100 K = 1, KDIMD
         DO J = 1, JDIMD
            ZM = 0.D0
            DO I = 1, IDIMD-1
               ZM = ZM + YDAT( I,J,K )/FLOAT( IDIMD-1 )
            ENDDO
            DO I = 1, IDIMD-1
               YDAT( I,J,K ) = YDAT( I,J,K ) - ZM
            ENDDO
         ENDDO
         write(6,* ) k
         WRITE( 91 )
     &     ( ( SNGL( YDAT(I,J,K) ) , I = 1, IDIMD-1 ), J = 1, JDIMD ) 
 2100 CONTINUE
      DO 2200 K = 1, KDIMD
         DO J = 1, JDIMD
            ZM = 0.D0
            DO I = 1, IDIMD-1
               ZM = ZM + ZDAT( I,J,K )/FLOAT( IDIMD-1 )
            ENDDO
            DO I = 1, IDIMD-1
               ZDAT( I,J,K ) = ZDAT( I,J,K ) - ZM
            ENDDO
         ENDDO
         write(6,* ) k
         WRITE( 91 )
     &     ( ( SNGL( ZDAT(I,J,K) ) , I = 1, IDIMD-1 ), J = 1, JDIMD ) 
 2200 CONTINUE
      DO J = 1, JDIMD
         ZM = 0.D0
         DO I = 1, IDIMD-1
            ZM = ZM + WDAT( I,J )/FLOAT( IDIMD-1 )
         ENDDO
         DO I = 1, IDIMD-1
            WDAT( I,J ) = WDAT( I,J ) - ZM
         ENDDO
      ENDDO
      WRITE( 91 )
     &     ( ( SNGL( WDAT(I,J) ) , I = 1, IDIMD-1 ), J = 1, JDIMD ) 
      CLOSE( 91 )
      call xabort( 1 )
*
      RETURN
      END
