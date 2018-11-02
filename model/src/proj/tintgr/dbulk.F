* PACKAGE DBULK   !! dynamics: bulk flux
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01 (SFCFLX)
*         01/07/06(hiro)     linearized flux for LBM2.0
*         01/07/10(hiro)     linear MCA after Neelin&Yu (1994,JAS)
*         02/08/15(hiro)     linear LSC
***********************************************************************
      SUBROUTINE SFCFLX    !!  surface flux
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [ENTRY OUTPUT] 
      REAL*8     GSFT  ( IJDIM, KMAX      )   !! steady heating
      REAL*8     GSFQ  ( IJDIM, KMAX      )   !! steady moistening
      REAL*8     GAFT  ( IJDIM, KMAX      )   !! heating, f(T')
      REAL*8     GAFQ  ( IJDIM, KMAX      )   !! moistening, f(Q')
      REAL*8     GAWT  ( IJDIM, KMAX      )   !! heating, f(W') 
      REAL*8     GAWQ  ( IJDIM, KMAX      )   !! cumulus moistening, f(W')
      REAL*8     GTAUX ( IJDIM            )   !! surface x-stress
      REAL*8     GTAUY ( IJDIM            )   !! surface y-stress
*
*   [ENTRY INPUT] 
      REAL*8     GAU   ( IJDIM, KMAX      )   !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX      )   !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX      )   !! temperature T
      REAL*8     GAQ   ( IJDIM, KMAX      )   !! humidity q
      REAL*8     GSST  ( IJDIM            )   !! SST anomaly
      REAL*8     GWG   ( IJDIM            )   !! soil wetness anomaly
      REAL*8     GIDX  ( IJDIM            )   !! surface index
*
*   [INPUT] 
      REAL*8     GFU   ( IJDIM, KMAX      )   !! westerly u
      REAL*8     GFV   ( IJDIM, KMAX      )   !! southern wind v
      REAL*8     GFT   ( IJDIM, KMAX      )   !! temperature T
      REAL*8     GFQ   ( IJDIM, KMAX      )   !! humidity q
      REAL*8     GFTV  ( IJDIM, KMAX      )   !! virt. temp.
      REAL*8     GFPS  ( IJDIM            )   !! sfc. pressure
      REAL*8     GFSST ( IJDIM            )   !! mean SST
      REAL*8     GFWG  ( IJDIM            )   !! soil wetness

      REAL*8     GDZS  ( IJDIM            )   !! topography
      REAL*8     DELT
*
*   [INTERNAL SAVE] 
      REAL*8     CMV   ( IJDIM )              !! bulk coef. u
      REAL*8     CHV   ( IJDIM )              !! bulk coef. T
      REAL*8     CEV   ( IJDIM )              !! bulk coef. q
      REAL*8     CMVF  ( IJDIM )              !! bulk trans. coef. u
      REAL*8     CHVF  ( IJDIM )              !! bulk trans. coef. T
      REAL*8     CEVF  ( IJDIM )              !! bulk trans. coef. q

      REAL*8     GDRHO ( IJDIM            )   !! air density
      REAL*8     VFABS ( IJDIM            )   !! wind speed
      REAL*8     DUABS ( IJDIM            )   !! dwind speed/du
      REAL*8     DVABS ( IJDIM            )   !! dwind speed/dv
      REAL*8     GFDT  ( IJDIM            )   !! mean temp. difference
      REAL*8     GFDQ  ( IJDIM            )   !! mean humi. difference
      REAL*8     GFWQ  ( IJDIM            )   !! mean evaporation

      REAL*8     EXNER ( IJDIM )         !! atmospheric Exner function
      REAL*8     EXNERM( IJDIM )         !! atmospheric Exner function
      REAL*8     QSAT  ( IJDIM )         !! surface saturate moisture
      REAL*8     DQSAT ( IJDIM )         !! change in qsat
      REAL*8     DELZ  ( IJDIM, KMAX )   !! delta Z for sfc level
      REAL*8     BETA  ( IJDIM )         !! evaporation efficiency

      REAL*8     GADT  ( IJDIM )         !! implicit damping
      REAL*8     GADQ  ( IJDIM )         !! implicit damping
      REAL*8     TTAUC ( IJDIM )
      REAL*8     QTAUC ( IJDIM )
      REAL*8     TFLGC ( IJDIM,KMAX )
      REAL*8     QFLGC ( IJDIM,KMAX )

      INTEGER    KBOT( IJDIM ), KTOP( IJDIM ), KB
*
*     LSC
*
      REAL*8     DQSATA( IJDIM, KMAX )   !! dQsat/dTa
      REAL*8     WTLSC ( IJDIM, KMAX )   !! weight for LSC
      REAL*8     TTAUL ( IJDIM )
      REAL*8     QTAUL ( IJDIM )
      REAL*8     TFLGL ( IJDIM,KMAX )
      REAL*8     QFLGL ( IJDIM,KMAX )

      SAVE       CMV, CHV, CEV, CMVF, CHVF, CEVF
      SAVE       GDRHO, VFABS, DUABS, DVABS
      SAVE       GFDT, GFDQ, EXNERM, EXNER, QSAT, DQSAT
      SAVE       DELZ, BETA
      SAVE       GADT, GADQ, GFWQ
      SAVE       TTAUC, QTAUC
      SAVE       TTAUL, QTAUL, DQSATA, WTLSC
      SAVE       TFLGC, TFLGL, QFLGC, QFLGL
      SAVE       KBOT, KTOP, KB
*
*   [INTERNAL WORK] 
      REAL*8     GDP   ( IJDIM            )   !! pressure
      REAL*8     GDPM  ( IJDIM            )   !! pressure (half lev)
      REAL*8     GDZ   ( IJDIM            )   !! altitude (half lev)
      REAL*8     DBETA ( IJDIM            )   !! dbeta/dW
      REAL*8     ZA    ( IJDIM            )   !! lowest layer altitude
      REAL*8     RIB   ( IJDIM            )   !! bulk Ri number
      REAL*8     FCTV  ( IJDIM            )   !! factor for wind speed 
      REAL*8     FCTQ  ( IJDIM            )   !! factor for humidity
      REAL*8     SIGW  ( KMAX  )
      REAL*8     SIGWT
      REAL*8     DT2
      REAL*8     WSPD
      REAL*8     AKAPPA
      INTEGER    IJ, K
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL PARM] 
      REAL*8     P00, THS
      REAL*8     URMIN, URMAX
      REAL*8     USMIN, USMAX
      REAL*8     Z0M, Z0H, Z0E
      REAL*8     TLAPS
      REAL*8     GFDTMX
      REAL*8     GFDTMN
      REAL*8     GFDQMX
      REAL*8     GFDQMN
      REAL*8     WGCRT          !! critical value for soil wetness
      REAL*8     ETLV           !! level for surface temperature
      REAL*8     EQLV           !! level for surface humidity
      REAL*8     EVLV           !! level for surface wind
      REAL*8     EXPW           !! exponent for sfc. heat redistribution
      LOGICAL    OWES           !! turn off the wind-evaporation feedback
      LOGICAL    OFIRST
      NAMELIST  /NMSFCM/ THS   , URMIN , URMAX,
     &                   USMIN , USMAX , 
     &                   Z0M, Z0H, Z0E , TLAPS,
     &                   GFDTMX , GFDTMN, GFDQMX, GFDQMN, WGCRT,
     &                   ETLV , EQLV, EVLV, EXPW, OWES

*   [INTERNAL FUNC] 
#include        "zqsat.F"                /* saturate q */
      DATA       P00   / 100000. /
      DATA       THS   / 300.    /            !! basic pot. temp
      DATA       URMIN / 0.5     /            !! min. wind vel. for Ri #
      DATA       URMAX / 1000.   /            !! max. wind vel. for Ri #
      DATA       USMIN / 2.5     /            !! min. wind vel. 
      DATA       USMAX / 1000.   /            !! max wind vel. 
      DATA       Z0M   / 1.D-3   /
      DATA       Z0H   / 1.D-3   /
      DATA       Z0E   / 1.D-3   /
      DATA       TLAPS  / 0.005  /            !! reduc. fact. to sea lev
      DATA       GFDTMX / 4.D0   /
      DATA       GFDTMN / 2.D0   /
      DATA       GFDQMX / 1.2D-3 /
      DATA       GFDQMN / 0.D0   /
      DATA       WGCRT  / 0.5D0  /
      DATA       ETLV   / 2.D0   /
      DATA       EQLV   / 2.D0   /
      DATA       EVLV   / 10.D0  /
      DATA       EXPW   / 1.D0   /
      DATA       OWES   / .TRUE. /
      DATA       OFIRST / .TRUE. /
*
*===================================================================
      ENTRY SETBULK
     I         ( GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GFTV  , GFSST , GFWG  , GDZS  , GIDX     )

      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ SETBULK: SURFACE FLUX 01/07/06'
         OFIRST = .FALSE.
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSFCM, END=190 )
  190    WRITE  ( JFPAR, NMSFCM )

         AKAPPA = RAIR / CP
         DO 1000 IJ = 1, IJDIM
            GDP ( IJ ) = GFPS( IJ ) * SIG( 1 ) * 100.  
            GDPM( IJ ) = GFPS( IJ ) * SIGM( 1 ) * 100.  
            GDZ ( IJ ) = GDZS( IJ ) 
     &                + RAIR * GFTV( IJ,1 ) / GRAV * ( 1.-SIG(1) )
            GDRHO( IJ ) = GDPM( IJ ) / ( RAIR * GFTV( IJ,1 ) )
            FCTV ( IJ ) = MIN( 1.D0, EVLV / ( GDZ( IJ ) - GDZS( IJ ) ) )
            FCTV ( IJ ) = FCTV( IJ )**0.3
            FCTQ ( IJ ) = MIN( 1.D0, EQLV / ( GDZ( IJ ) - GDZS( IJ ) ) )
            FCTQ ( IJ ) = FCTQ( IJ )**0.6
            VFABS( IJ ) = SQRT( GFU( IJ,1 )**2 + GFV( IJ,1 )**2  )
            VFABS( IJ ) = FCTV( IJ ) * VFABS( IJ )
            VFABS( IJ ) = MIN( MAX ( VFABS( IJ ), USMIN ), USMAX )
            DUABS( IJ ) = GFU( IJ,1 ) * FCTV( IJ )**2 / VFABS( IJ )
            DVABS( IJ ) = GFV( IJ,1 ) * FCTV( IJ )**2 / VFABS( IJ )
            EXNER ( IJ ) = ( GDP ( IJ )/ P00 ) ** AKAPPA
            EXNERM( IJ ) = ( GDPM( IJ )/ P00 ) ** AKAPPA
            BETA ( IJ ) = MIN( 1.D0, SQRT( GFWG( IJ )/WGCRT ) )
            IF( GIDX( IJ ) .EQ. 0.D0 ) BETA( IJ ) = 1.D0
            IF( BETA( IJ ) .GE. 1.D0 ) THEN
               DBETA( IJ ) = 0.D0
            ELSEIF( BETA( IJ ) .LT. 0.4D0 ) THEN
               DBETA( IJ ) = 0.D0
            ELSE
               DBETA( IJ ) = 1.D0 / SQRT( 
     &                     ( 4.D0 * GFWG( IJ ) * WGCRT ) )
            ENDIF

            QSAT ( IJ ) = FQSAT ( GFSST( IJ ), GDPM( IJ ) )
            DQSAT( IJ ) = FDQSAT( GFSST( IJ ), QSAT( IJ )   )
            GFDT ( IJ ) = MAX( GFDTMN , MIN( GFDTMX , 
     &                    GFSST( IJ )  /EXNERM( IJ )
     &                  - GFT  ( IJ,1 )/EXNER ( IJ ) ) )
            GFDQ ( IJ ) = ( QSAT ( IJ )  - GFQ( IJ,1 ) ) * FCTQ( IJ ) 
            GFDQ ( IJ ) = MAX( GFDQMN, MIN( GFDQMX, GFDQ( IJ ) ) )
            ZA   ( IJ ) = GDZ( IJ ) - GDZS( IJ )
*
*     no temp. difference over land and ice
*
            IF( GIDX( IJ ) .NE. 0.D0 .OR. BETA( IJ ) .LT. 1.D0 ) 
     &           GFDT( IJ ) = 0.D0
*
            RIB  ( IJ ) = - GRAV / THS * GFDT( IJ ) / 
     &           MIN( MAX( VFABS( IJ )**2, URMIN), URMAX ) 
     &           * ZA( IJ )
 1000    CONTINUE
*
*     from MCA setup
*
         CALL GETMCAT 
     O     ( TTAUC , QTAUC , KBOT  , KTOP   )
         KB = KBOT ( 1 )
         CALL GETFLGC ( TFLGC , QFLGC )
*
*     from LSC setup
*
         CALL GETLSCT 
     O     ( TTAUL , QTAUL , DQSATA , WTLSC   )
         CALL GETFLGL ( TFLGL , QFLGL )
*
         SIGWT = 0.D0
         DO 1100 K = 1, KB
            SIGW( K ) = ( 1.D0 / ( 1.D0 - SIG( K ) ) )**EXPW
            SIGWT     = SIGWT + SIGW( K )
 1100    CONTINUE
         DO 1150 K = 1, KB
            SIGW( K ) = SIGW( K ) / SIGWT
 1150    CONTINUE
*
         DO 1200 K = 1, KB
            DO 1200 IJ = 1, IJDIM
               DELZ ( IJ,K ) = 
     &              GFPS( IJ )*1.D2 * DSIG( K ) / GRAV / SIGW( K )
 1200    CONTINUE
*
         CALL BLKCOF
     O         ( CMV   , CHV   , CEV   ,
     I           RIB   , VFABS  , 
     I           Z0M   , Z0H   , Z0E   , ZA    )
*
*     bulk tranfer coefficients etc.
*
         DO 1300 IJ = 1, IJDIM
            CMVF  ( IJ ) = CMV ( IJ ) * VFABS( IJ ) * FCTV( IJ )
            CHVF  ( IJ ) = CHV ( IJ ) * VFABS( IJ )
            CEVF  ( IJ ) = CEV ( IJ ) * VFABS( IJ )
            GADT  ( IJ ) = CHVF( IJ ) 
     &                   * EXNERM( IJ ) / EXNER( IJ )
            GADQ  ( IJ ) = CEVF( IJ )
            GFWQ  ( IJ ) = CEVF( IJ ) * GFDQ( IJ ) * DBETA( IJ )
 1300    CONTINUE
*
      ENDIF
*
      RETURN
*===================================================================
      ENTRY    TS2TQF    !! SST -> Fh,Fq
     O     ( GSFT , GSFQ ,
     I       GSST           )
*
*
      DO 2000 K = 1, KB
         DO 2000 IJ = 1, IJDIM
            GSFT( IJ,K ) = CHVF( IJ ) * EXNERM( IJ )
     &                   * GSST( IJ ) / EXNERM( IJ ) / DELZ( IJ,K )
            GSFQ( IJ,K ) = CEVF( IJ ) * DQSAT( IJ )
     &                   * GSST( IJ ) * BETA( IJ ) / DELZ( IJ,K )
 2000 CONTINUE
*
      RETURN
*===================================================================
      ENTRY    WG2TQF    !! SST -> Fh,Fq
     M     ( GSFT , GSFQ ,
     I       GWG            )
*
*
      DO 2100 K = 1, KB
         DO 2100 IJ = 1, IJDIM
            GSFQ( IJ,K ) = GSFQ( IJ,K ) 
     &                   + GFWQ( IJ ) * GWG( IJ ) / DELZ( IJ,K )
 2100 CONTINUE
*
      RETURN
*===================================================================
      ENTRY    UV2TQF           !! Ta,Qa,Va -> Fh,Fq
     O     ( GAFT , GAFQ , GAWT , GAWQ ,
     I       GAU  , GAV  , GAT  , GAQ    )
*
*
      CALL RESET( GAFT , IJDIM*KMAX )
      CALL RESET( GAFQ , IJDIM*KMAX )
      CALL RESET( GAWT , IJDIM*KMAX )
      CALL RESET( GAWQ , IJDIM*KMAX )
*
      DO 3000 K = 1, KB
         DO 3000 IJ = 1, IJDIM
*
*     wind-evaporation feedback
*
            GAWT( IJ,K ) = CHV ( IJ ) * 
     &                   ( DUABS( IJ ) * GAU( IJ,1 ) 
     &                   + DVABS( IJ ) * GAV( IJ,1 ) ) 
     &                   * GFDT( IJ ) / DELZ( IJ,K )
            GAWQ( IJ,K ) = CEV ( IJ ) *
     &                   ( DUABS( IJ ) * GAU( IJ,1 ) 
     &                   + DVABS( IJ ) * GAV( IJ,1 ) ) 
     &                   * GFDQ( IJ ) * BETA( IJ ) / DELZ( IJ,K )
*
*     damping
*
            GAFT( IJ,K ) = - GADT( IJ ) * GAT( IJ,1 ) / DELZ( IJ,K )
            GAFQ( IJ,K ) = - GADQ( IJ ) * GAQ( IJ,1 )
     &                     * BETA( IJ ) / DELZ( IJ,K )
 3000 CONTINUE
*
      IF ( .NOT. OWES ) THEN
         CALL RESET( GAWT , IJDIM*KMAX )
         CALL RESET( GAWQ , IJDIM*KMAX )
         CALL RESET( GAFT , IJDIM*KMAX )
         CALL RESET( GAFQ , IJDIM*KMAX )
      ENDIF
*
      RETURN
*===================================================================
      ENTRY    IMPTQ    !! implicit damping for Ta,Qa
     M     (  GAT  , GAQ  ,
     I        DELT         )
*
*
      DT2 = DELT * 2.D0
      DO 4000 K = 1, KB
         DO 4000 IJ = 1, IJDIM
            GAT( IJ,K ) = GAT( IJ,K ) /
     &           ( 1.D0 + DT2 * GADT( IJ )              / DELZ( IJ,K ) )
            GAQ( IJ,K ) = GAQ( IJ,K ) /
     &           ( 1.D0 + DT2 * GADQ( IJ ) * BETA( IJ ) / DELZ( IJ,K ) )
 4000 CONTINUE
*
#ifdef OPT_POSDEF
      CALL GETFLGC ( TFLGC , QFLGC )
#endif
*
      DO 4100 IJ = 1, IJDIM
         DO 4100 K = KB, KTOP( IJ )
            GAT( IJ,K ) =   GAT( IJ,K ) /
     &           ( 1.D0 + DT2 * TTAUC( IJ ) * TFLGC( IJ,K ) )
            GAQ( IJ,K ) =   GAQ( IJ,K ) /
     &           ( 1.D0 + DT2 * QTAUC( IJ ) * QFLGC( IJ,K ) )
 4100 CONTINUE
*
*     LSC
*
#ifdef OPT_POSDEF
      CALL GETFLGL ( TFLGL , QFLGL )
#endif
*
      DO 4200 K = 1, KMAX
         DO 4200 IJ = 1, IJDIM
            GAT( IJ,K ) =   GAT( IJ,K ) /
     &           ( 1.D0 + DT2 * TTAUL( IJ ) * WTLSC( IJ,K )
     &             * DQSATA( IJ,K ) * TFLGL( IJ,K ) * EL / CP  )
            GAQ( IJ,K ) =   GAQ( IJ,K ) /
     &           ( 1.D0 + DT2 * QTAUL( IJ ) * WTLSC( IJ,K )
     &             * QFLGL( IJ,K )                             )
 4200 CONTINUE
*
*
      RETURN
*===================================================================
      ENTRY   UV2TAU            !! U,V -> sfc stress
     O     ( GTAUX , GTAUY ,
     I       GFU   , GFV   , GAU   , GAV     )
*
*     wind stresses
*
      DO 5000 IJ = 1, IJDIM
         WSPD        = ( DUABS( IJ ) * GAU( IJ,1 ) 
     &                + DVABS( IJ ) * GAV( IJ,1 ) ) 
         GTAUX( IJ ) =   GDRHO( IJ ) * 
     &                ( CMVF( IJ ) * GAU( IJ,1 ) + WSPD * GFU( IJ,1 ) )
         GTAUY( IJ ) =   GDRHO( IJ ) * 
     &                ( CMVF( IJ ) * GAV( IJ,1 ) + WSPD * GFV( IJ,1 ) )
 5000 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE BLKCOF    !! bulk coefficient
     O         ( CM    , CH    , CE    ,
     I           RIB   , VABS  , 
     I           Z0M   , Z0H   , Z0E   , ZA    )
*
*          Louis Bulk Coeff.
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     CM    ( IJSDIM )         !! bulk coefficient : u
      REAL*8     CH    ( IJSDIM )         !! bulk coefficient : T
      REAL*8     CE    ( IJSDIM )         !! bulk coefficient : q
*
*   [INPUT] 
      REAL*8     RIB   ( IJSDIM )         !! bulk Ri number
      REAL*8     VABS  ( IJSDIM )         !! wind speed
      REAL*8     Z0M
      REAL*8     Z0H
      REAL*8     Z0E
      REAL*8     ZA    ( IJSDIM )         !! altitude of lowest level
*
*   [INTERNAL WORK] 
      REAL*8     RIB0, T0TH, Q0QE, FCM, FCH
      REAL*8     CMX, FCX, RIX
      REAL*8     FM, FH, FZH, FZE
      INTEGER    IJ
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL PARM] 
      REAL*8     CHCONS
      LOGICAL    NEWTRL
      REAL*8     CMMIN, CHMIN, CEMIN
      REAL*8     CMMAX, CHMAX, CEMAX
      REAL*8     FB   , FBS  , FDM  , FDH
      LOGICAL    OFIRST
      NAMELIST  /NMSFCL/ CHCONS, NEWTRL, 
     &                   CMMIN , CHMIN , CEMIN ,
     &                   CMMAX , CHMAX , CEMAX ,
     &                   FB    , FBS   , 
     &                   FDM   , FDH   
      DATA       CHCONS /  -1.   /
      DATA       NEWTRL / .FALSE. /
      DATA       CMMIN  /  1.E-5 /           !! min. bulk coef. of u
      DATA       CHMIN  /  1.E-5 /           !! min. bulk coef. of T
      DATA       CEMIN  /  1.E-5 /           !! min. bulk coef. of q
      DATA       CMMAX  /   1.   /           !! max. bulk coef. of u
      DATA       CHMAX  /   1.   /           !! max. bulk coef. of T
      DATA       CEMAX  /   1.   /           !! max. bulk coef. of q
      DATA       FB     /  9.4   /           !! Louis factor b 
      DATA       FBS    /  4.7   /           !! Louis factor b'
      DATA       FDM    /  7.4   /           !! Louis factor d of u
      DATA       FDH    /  5.3   /           !! Louis factor d of T,q
      DATA       OFIRST / .TRUE. /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ BULKCOF: SURFACE BULK COEF. Louis 96/11/25'
         OFIRST = .FALSE.
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSFCL, END=190 )
  190    WRITE  ( JFPAR, NMSFCL )
      ENDIF
*
*   < constant case >
*
      IF ( CHCONS .GT. 0. ) THEN
         DO 1100 IJ = 1, IJSDIM
            CM( IJ ) = CHCONS
            CH( IJ ) = CHCONS
            CE( IJ ) = CHCONS
 1100    CONTINUE
         RETURN
      ENDIF
*
*   < neutral case >
*
      IF ( NEWTRL ) THEN
         DO 2100 IJ = 1, IJSDIM
            CMX      = FKARM**2 / LOG( ZA( IJ )/Z0M )
            CM( IJ ) = CMX      / LOG( ZA( IJ )/Z0M )
            CH( IJ ) = CMX      / LOG( ZA( IJ )/Z0H )
            CE( IJ ) = CMX      / LOG( ZA( IJ )/Z0E )
 2100    CONTINUE
         GOTO 5000
      ENDIF
*
*   < non-neutral case >
*
      DO 3100 IJ = 1, IJSDIM
         CM( IJ ) = FKARM**2 / LOG( ZA( IJ )/Z0M )**2
         CH( IJ ) = CM( IJ )
         CE( IJ ) = CM( IJ )
 3100 CONTINUE
*
      DO 3200 IJ = 1, IJSDIM
         FCX  = FB  * SQRT( ZA( IJ )/Z0M )
         FCM  = FDM * CM( IJ ) * FCX
         FCH  = FDH * CM( IJ ) * FCX
*
         FZH =   LOG( Z0M/Z0H )
     &         / LOG( ZA( IJ )   /Z0M )
         FZE =   LOG( Z0M/Z0E )
     &         / LOG( ZA( IJ )   /Z0M )
*
         T0TH = 1.D0/( 1.D0 + FZH )
         RIB0 = RIB( IJ ) * T0TH
*
         IF ( RIB( IJ ) .GE. 0.D0 ) THEN
            FM  = 1.D0/( 1.D0 + FBS * RIB0 )**2
            FH  = FM
         ELSE
            RIX = SQRT( ABS( RIB0 ) )
            FM  = 1.D0 - FB*RIB0/( 1.D0 + FCM*RIX )
            FH  = 1.D0 - FB*RIB0/( 1.D0 + FCH*RIX )
         ENDIF
*
         T0TH = 1.D0/( 1.D0 + FZH/SQRT( FM )*FH )
         Q0QE = 1.D0/( 1.D0 + FZE/SQRT( FM )*FH )
         RIB0 = RIB( IJ ) * T0TH
*
         IF ( RIB( IJ ) .GE. 0.D0 ) THEN
            FM  = 1.D0/( 1.D0 + FBS * RIB0 )**2
            FH  = FM
         ELSE
            RIX = SQRT( ABS( RIB0 ) )
            FM  = 1.D0 - FB*RIB0/( 1.D0 + FCM*RIX )
            FH  = 1.D0 - FB*RIB0/( 1.D0 + FCH*RIX )
         ENDIF
*
         CM( IJ ) = CM( IJ ) * FM
         CH( IJ ) = CH( IJ ) * FH * T0TH
         CE( IJ ) = CE( IJ ) * FH * Q0QE
 3200 CONTINUE 
*
*   < max, min >  
*
 5000 CONTINUE 
      DO 5100 IJ = 1, IJSDIM
         CM( IJ ) = MAX( MIN( CM( IJ ), CMMAX ), CMMIN )
         CH( IJ ) = MAX( MIN( CH( IJ ), CHMAX ), CHMIN )
         CE( IJ ) = MAX( MIN( CE( IJ ), CEMAX ), CEMIN )
 5100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE MCA            !!  convective adjustment
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [ENTRY OUTPUT] 
      REAL*8     GAFT  ( IJDIM, KMAX      )   !! heating, f(T')
      REAL*8     GAFQ  ( IJDIM, KMAX      )   !! moistening, f(Q')
      REAL*8     GAWT  ( IJDIM, KMAX      )   !! cumulus heating, f(W') 
      REAL*8     GAWQ  ( IJDIM, KMAX      )   !! cumulus moistening, f(W')
*
*   [ENTRY INPUT] 
      REAL*8     GAT   ( IJDIM, KMAX      )   !! temperature T
      REAL*8     GAQ   ( IJDIM, KMAX      )   !! humidity q
      REAL*8     GIDX  ( IJDIM            )   !! surface index
*
*   [INPUT] 
      REAL*8     GFT   ( IJDIM, KMAX      )   !! temperature T
      REAL*8     GFQ   ( IJDIM, KMAX      )   !! humidity q
      REAL*8     GFTV  ( IJDIM, KMAX      )   !! virt. temp.
      REAL*8     GFPS  ( IJDIM            )   !! sfc. pressure
      REAL*8     GDZS  ( IJDIM            )   !! topography
*
*   [INTERNAL SAVE] 
      REAL*8     TTAU  ( IJDIM      )    !! T-adjust timescale
      REAL*8     QTAU  ( IJDIM      )    !! q-adjust timescale
      REAL*8     TTAC  ( IJDIM      )    !! T-correction timescale
      REAL*8     QTAC  ( IJDIM      )    !! q-correction timescale
      REAL*8     DQSAT ( IJDIM,KMAX )    !! change in qsat
      REAL*8     ALP   ( IJDIM,KMAX )    !! relative humidity
      REAL*8     AREF  ( IJDIM,KMAX )    !! wighting profile A
      REAL*8     AREFC ( IJDIM,KMAX )    !! cloud top correction
      REAL*8     AREFV ( IJDIM      )    !! vertical average of A
      REAL*8     BREF  ( IJDIM,KMAX )    !! wighting profile B
      REAL*8     BREFV ( IJDIM      )    !! vertical average of B
      REAL*8     GCEFF ( IJDIM      )    !! convective efficiency
      REAL*8     DSIGT ( IJDIM,KMAX )
      REAL*8     DTCW  ( IJDIM,KMAX )    !! weight for delta Tc
      REAL*8     TFLG  ( IJDIM,KMAX )    !! flag for Q1
      REAL*8     QFLG  ( IJDIM,KMAX )    !! flag for Q2
      REAL*8     GQBS  ( IJDIM,KMAX )    !! basic state q
      REAL*8     AKAPPA
      INTEGER    KTOP  ( IJDIM )         !! cloud top
      INTEGER    KB                      !! level of PBL top

      SAVE       DQSAT, ALP, AREF, AREFV, BREF, BREFV
      SAVE       TTAU, QTAU, TTAC, QTAC, GCEFF
      SAVE       AKAPPA, DSIGT, DTCW
      SAVE       TFLG, QFLG
      SAVE       GQBS
      SAVE       KB, KTOP
*
*   [INTERNAL WORK]
      REAL*8     GFZ   ( IJDIM,KMAX )    !! geopotential height
      REAL*8     GFHS  ( IJDIM,KMAX )    !! saturate moist static energy  
      REAL*8     GFHB  ( IJDIM )         !! moist static energy at PBL
      REAL*8     CAPE  ( IJDIM )         !! CAPE
      REAL*8     HB    ( IJDIM )         !! moist static energy at PBL
      REAL*8     DTC   ( IJDIM )         !! correction term for Tc
      REAL*8     DQC   ( IJDIM )         !! correction term for qc
      REAL*8     TV    ( IJDIM )         !! vertical average of Ta
      REAL*8     QV    ( IJDIM )         !! vertical average of Qa
      REAL*8     GTV   ( IJDIM )   
      REAL*8     GDPM, QSAT
      REAL*8     TTAUX ( IJDIM )
      REAL*8     QTAUX ( IJDIM )
      REAL*8     TFLGX ( IJDIM,KMAX )
      REAL*8     QFLGX ( IJDIM,KMAX )
      REAL*8     TTAU1, QTAU1, TTAC1, QTAC1
      REAL*8     GAMMA0
      REAL*8     GAMMA
      REAL*8     DELH
      REAL*8     DSIGA
      REAL*8     DQ1, DQ2, Q1C, Q2C, DTD, DQD
      INTEGER    IJ, K, KK, K1, K2
      INTEGER    KBT( IJDIM ), KTP( IJDIM )
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL PARM] 
      REAL*8     TTAU0, QTAU0   !! relaxation time due to MCA
      REAL*8     TTAUC          !! relaxation time for correction
      REAL*8     SIGKB          !! level of PBL top
      REAL*8     THCAPE         !! CAPE threshold for ec
      REAL*8     CEFMX          !! maximum value for ec
      REAL*8     DSEC
      REAL*8     FLND
      LOGICAL    OCUM           !! apply MCA
      LOGICAL    OCEF           !! use Ce limit
      LOGICAL    OFIRST
      CHARACTER  TUNIT*4        !! unit of time
      NAMELIST  /NMMCA/  OCUM, TTAU0, QTAU0, TTAUC, TUNIT, 
     &                   SIGKB, THCAPE, OCEF, CEFMX, FLND
*   [INTERNAL FUNC] 
#include        "zqsat.F"                /* saturate q */
      DATA       TTAU0  / 2.D0   /
      DATA       QTAU0  / 2.D0   /
      DATA       TTAUC  / 12.D0  /
      DATA       TUNIT  / 'HOUR' /
      DATA       SIGKB  / 0.9D0  /
      DATA       THCAPE / 3.D3   /
      DATA       CEFMX  / 0.9D0  /
      DATA       DSEC   / 0.D0   /
      DATA       FLND   / 1.D0   /
      DATA       OCUM   / .TRUE. /
      DATA       OCEF   / .FALSE./
      DATA       OFIRST / .TRUE. /
*
*===================================================================
      ENTRY SETMCA
     I         ( GFT  , GFQ  , GFTV , GFPS  , GDZS  , GIDX  )


      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ SETMCA: CONVECTIVE ADJUSTMENT 01/07/09'
         OFIRST = .FALSE.
#ifdef OPT_POSDEF
         WRITE ( 6,* ) ' ### POSITIVE DEFINITE Q1/Q2 (NONLIN) '
#endif
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMMCA, END=190 )
  190    WRITE  ( JFPAR, NMMCA )

         CALL CXX2SS ( TTAU1, TTAU0, TUNIT, DSEC  )
         CALL CXX2SS ( QTAU1, QTAU0, TUNIT, DSEC  )
         CALL CXX2SS ( TTAC1, TTAUC, TUNIT, DSEC  )
         TTAU1 = 1.D0 / TTAU1
         QTAU1 = 1.D0 / QTAU1
         TTAC1 = 1.D0 / TTAC1
         QTAC1 = TTAC1
         CALL RESET( TTAU , IJDIM )
         CALL RESET( QTAU , IJDIM )
         CALL RESET( TTAC , IJDIM )
         CALL RESET( QTAC , IJDIM )
         IF( OCUM ) THEN
            DO 500 IJ = 1, IJDIM
               TTAU( IJ ) = TTAU1
               QTAU( IJ ) = QTAU1
               TTAC( IJ ) = TTAC1
               QTAC( IJ ) = QTAC1
               IF( GIDX( IJ ) .GT. 0.D0 ) THEN
                  TTAU( IJ ) = TTAU1 * FLND
                  QTAU( IJ ) = QTAU1 * FLND
                  TTAC( IJ ) = TTAC1 * FLND
                  QTAC( IJ ) = QTAC1 * FLND
               ENDIF
 500        CONTINUE
         ENDIF

         CALL GHYDRO
     O         ( GFZ   ,
     I           GFTV  , GDZS  )
*
*     cloud bottom (fixed) and top (diagnosed)
*
         AKAPPA = RAIR / CP
         KB = 1
         DO 700 K = 1, KMAX
            IF( SIG( K ) .GT. SIGKB ) KB = K 
  700    CONTINUE
         DO 800 K = 1, KMAX
            DO 800 IJ = 1, IJDIM
               GDPM  = GFPS( IJ ) * SIGM( K ) * 100.  
               QSAT  = FQSAT ( GFT( IJ,K ), GDPM         )
               GFHS( IJ,K ) = CP   * GFT( IJ,K ) 
     &                     + GRAV * GFZ( IJ,K ) 
     &                     + EL   * QSAT
               DQSAT( IJ,K ) = FDQSAT( GFT( IJ,K ), QSAT )
               ALP( IJ,K ) = MIN( 1.D0, GFQ( IJ,K )/QSAT )
 800     CONTINUE
         DO 900 IJ = 1, IJDIM
            GFHB( IJ ) = CP   * GFT( IJ,1 ) 
     &                 + GRAV * GFZ( IJ,1 ) 
     &                 + EL   * GFQ( IJ,1 ) 
 900     CONTINUE
         CALL KCTOP( KTOP , CAPE , GFHS , GFHB )
         DO 1000 IJ = 1, IJDIM
            DO 1000 K = 1, KMAX
               K1 = MIN( KMAX, KTOP( IJ ) + 1 )
               DSIGT( IJ,K ) = MIN( 1.D0, DSIG( K ) / 
     &              MAX( 1.D-2, ( SIGM( KB ) - SIGM( K1 ) ) ) )
 1000    CONTINUE
*
*     convective efficiency
*
         IF( OCEF ) THEN
            DO 1100 IJ = 1, IJDIM
               GCEFF( IJ ) = MIN( CEFMX, CAPE( IJ ) / THCAPE )
 1100       CONTINUE
         ELSE
            CALL SETV  ( GCEFF , 1.D0 , IJDIM )
         ENDIF
*
*     correction for A near the cloud top
*
         DO 1200 IJ = 1, IJDIM
            DO 1200 K = 1, MIN( KTOP( IJ )+1, KMAX )
               K1 = KTOP( IJ ) + 1
               K2 = MAX( 1, KTOP( IJ ) - 1 )
               DSIGA = ( SIGM( K  ) - SIGM( K1 ) )
     &               / ( SIGM( K2 ) - SIGM( K1 ) )
               AREFC( IJ,K ) = MIN( 1.D0, SQRT( DSIGA ) )
 1200    CONTINUE
*
*     reference profiles
*
         CALL RESET ( AREF  , IJDIM*KMAX )
         CALL RESET ( AREFV , IJDIM      )
         CALL RESET ( BREF  , IJDIM*KMAX )
         CALL RESET ( BREFV , IJDIM      )
         CALL RESET ( TFLG  , IJDIM*KMAX )
         CALL RESET ( QFLG  , IJDIM*KMAX )
         CALL SETV  ( DTCW  , 1.D0, IJDIM*KMAX )
         CALL COPY  ( GQBS  , GFQ , IJDIM*KMAX )
         DO 1300 IJ = 1, IJDIM
            DO 1400 K = KB, KTOP( IJ )
               DO 1500 KK = KB, K
                  GAMMA0 = DQSAT( IJ,KK ) * EL / CP
                  GAMMA  = 1.D0 / ( 1.D0 + GAMMA0 )
                  AREF( IJ,K ) = AREF( IJ,K ) + 
     &                           GAMMA * DSIG( KK ) / SIG( KK )
 1500          CONTINUE
               GAMMA0 = DQSAT( IJ,K ) * EL / CP
               GAMMA  = 1.D0 / ( 1.D0 + GAMMA0 )
               AREF( IJ,K ) = AREFC( IJ,K ) * GCEFF( IJ ) *
     &                        GAMMA * EXP( -AKAPPA * AREF( IJ,K ) )
               BREF( IJ,K ) = GAMMA0 * AREF( IJ,K )
               AREFV( IJ )  = AREFV( IJ ) + AREF( IJ,K ) * DSIGT( IJ,K )
               BREFV( IJ )  = BREFV( IJ ) + BREF( IJ,K ) * DSIGT( IJ,K )
 1400       CONTINUE
*
            DO 1600 K = KB, KTOP( IJ )
               K1 = MIN( KMAX, KTOP( IJ ) + 1 )
               DSIGA = MAX( 1.D-2, ( SIGM( KB ) - SIGM( K1 ) ) )
               IF( AREFV( IJ ) .NE. 0.D0 ) THEN
                  DTCW( IJ,K ) = 
     &                 AREF( IJ,K ) * DSIGT( IJ,K ) * ( K1-KB )
     &                           / AREFV( IJ ) 
               ENDIF
               TFLG( IJ,K ) = 1.D0
               QFLG( IJ,K ) = 1.D0
 1600       CONTINUE
 1300    CONTINUE
*
         CALL STDOUT( AREFV , 'Aref' , IDIM , JDIM , 1  , 1.D1 )
*
      ENDIF
*
      RETURN
*===================================================================
      ENTRY    TQ2TQF    !! Ta,Qa -> Fh and Fq due to MCA
     M     ( GAFT , GAFQ , GAWT , GAWQ ,
     I       GAT  , GAQ    )
*
*     vertical averages
*
      CALL RESET( TV  , IJDIM )
      CALL RESET( QV  , IJDIM )
      CALL RESET( HB  , IJDIM )
      DO 2000 IJ = 1, IJDIM
         DO 2000 K = KB, KTOP( IJ )
            GAMMA0    = DQSAT( IJ,K ) * EL / CP
            TV ( IJ ) = TV ( IJ ) + GAT( IJ,K ) * DSIGT( IJ,K )
            QV ( IJ ) = QV ( IJ ) + GAQ( IJ,K ) * DSIGT( IJ,K )
            GTV( IJ ) = GTV( IJ ) + ALP( IJ,K ) * 
     &                  GAMMA0 * GAT( IJ,K ) * DSIGT( IJ,K )
 2000 CONTINUE
*
*     moist static energy at PBL
*
      DO 2100 IJ = 1, IJDIM
         HB ( IJ )  = CP * GAT( IJ,1 ) + EL * GAQ( IJ,1 ) 
 2100 CONTINUE
*
*     correction terms
*
      DO 2200 IJ = 1, IJDIM
         DELH       = ( AREFV( IJ ) + BREFV( IJ ) ) * HB( IJ ) 
     &                - CP * TV( IJ ) - EL * QV( IJ ) 
         DTC( IJ ) = DELH / CP
         DQC( IJ ) = 0.D0
 2200 CONTINUE
*
*     heat source & moisture sink
*
      DO 2300 IJ = 1, IJDIM
         DO 2300 K = KB, KTOP( IJ )
            DQ1 = TTAU( IJ ) * AREF( IJ,K ) * HB( IJ ) / CP
            DQ2 = QTAU( IJ ) * BREF( IJ,K ) * HB( IJ ) / EL
            Q1C = - TTAC( IJ )*DTC( IJ )*DTCW( IJ,K )*GCEFF( IJ )
            Q2C = - QTAC( IJ ) * DQC( IJ ) * GCEFF( IJ )
            DTD = - TTAU( IJ ) * GCEFF( IJ ) * GAT( IJ,K )
            DQD = - QTAU( IJ ) * GCEFF( IJ ) * GAQ( IJ,K )
#ifdef OPT_POSDEF
CCX            IF( GAQ(IJ,K)+GQBS(IJ,K) .GT. 0.D0  ) THEN
CCX            IF( DQ1+Q1C+DTD          .GT. 0.D0        ) THEN
            IF( GAQ(IJ,K)+GQBS(IJ,K) .GT. 0.D0 .AND.
     &          DQ1+Q1C+DTD          .GT. 0.D0        ) THEN
#endif
            GAWT( IJ,K ) = GAWT( IJ,K ) + DQ1 + Q1C
            GAWQ( IJ,K ) = GAWQ( IJ,K ) + DQ2 + Q2C
            GAFT( IJ,K ) = GAFT( IJ,K ) + DTD
            GAFQ( IJ,K ) = GAFQ( IJ,K ) + DQD
#ifdef OPT_POSDEF
               TFLG( IJ,K ) = 1.D0
               QFLG( IJ,K ) = 1.D0
            ELSE
               TFLG( IJ,K ) = 0.D0
               QFLG( IJ,K ) = 0.D0
            ENDIF
#endif
 2300 CONTINUE
*
*
      RETURN
*===================================================================
      ENTRY GETMCAT
     O         ( TTAUX , QTAUX , KBT , KTP  )
*
      IF( OFIRST ) THEN
         WRITE ( 6,* ) ' ### ERROR: SETMCA MUST BE CALLED FIRST ### '
         CALL XABORT( 1 )
      ENDIF

      DO 2500 IJ = 1, IJDIM
         TTAUX( IJ ) = TTAU( IJ ) * GCEFF( IJ )
         QTAUX( IJ ) = QTAU( IJ ) * GCEFF( IJ )
         KBT  ( IJ ) = KB
         KTP  ( IJ ) = KTOP( IJ )
 2500 CONTINUE
*
      RETURN
*===================================================================
      ENTRY GETFLGC
     O         ( TFLGX , QFLGX  )
*
      IF( OFIRST ) THEN
         WRITE ( 6,* ) ' ### ERROR: SETMCA MUST BE CALLED FIRST ### '
         CALL XABORT( 1 )
      ENDIF

      DO 2600 K = 1, KMAX
         DO 2600 IJ = 1, IJDIM
            TFLGX( IJ,K ) = TFLG( IJ,K )
            QFLGX( IJ,K ) = QFLG( IJ,K )
 2600 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE KCTOP          !!  estimate cloud top level
     O     ( KTOP  , CAPE  , 
     I       GFHS  , GFHB    )
*
*     cloud top is defined by the level with H*=Hb*
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      INTEGER    KTOP  ( IJDIM        )     !! cloud top level
      REAL*8     CAPE  ( IJDIM        )     !! CAPE
*
*   [INPUT] 
      REAL*8     GFHS  ( IJDIM,KMAX   )     !! saturated moist static energy
      REAL*8     GFHB  ( IJDIM        )     !! moist static energy at PBL
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
*   [INTERNAL PARAM] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ KCTOP: LEVEL AT ZERO BUOYANCY 01/08/22'
         OFIRST = .FALSE.
         CALL RESET( CAPE , IJDIM )
      ENDIF
*
      DO 1200 IJ = 1, IJDIM
         KTOP( IJ ) = 1
 1200 CONTINUE
*
      DO 1300 IJ = 1, IJDIM
         DO 1400 K = KMAX-1, 1, -1
            IF( GFHS( IJ,K ) .LE. GFHB( IJ ) ) THEN
               KTOP( IJ ) = MAX( K+1, KTOP( IJ ) )
               GOTO 1300
            ENDIF
 1400    CONTINUE
 1300 CONTINUE
*
      DO 1500 IJ = 1, IJDIM
         DO 1600 K = KTOP( IJ ), 1, -1
            IF( GFHS( IJ,K ) .LE. GFHB( IJ ) ) THEN
               CAPE( IJ ) = CAPE( IJ ) + MAX( 0.D0,  
     &                    ( GFHB( IJ ) - GFHS( IJ,K ) ) * DSIG( K ) )
            ENDIF
 1600    CONTINUE
 1500 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE LSC            !!  large scale condensation
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [ENTRY OUTPUT] 
      REAL*8     GAFT  ( IJDIM, KMAX      )   !! heating, f(T')
      REAL*8     GAFQ  ( IJDIM, KMAX      )   !! moistening, f(Q')
      REAL*8     GAWT  ( IJDIM, KMAX      )   !! LSC heating, f(W') 
      REAL*8     GAWQ  ( IJDIM, KMAX      )   !! LSC moistening, f(W')
*
*   [ENTRY INPUT] 
      REAL*8     GAT   ( IJDIM, KMAX      )   !! temperature T
      REAL*8     GAQ   ( IJDIM, KMAX      )   !! humidity q
      REAL*8     GIDX  ( IJDIM            )   !! surface index
*
*   [INPUT] 
      REAL*8     GFT   ( IJDIM, KMAX      )   !! temperature T
      REAL*8     GFQ   ( IJDIM, KMAX      )   !! humidity q
      REAL*8     GFPS  ( IJDIM            )   !! sfc. pressure
*
*   [INTERNAL SAVE] 
      REAL*8     RHBS  ( IJDIM,KMAX )    !! basic state RH
      REAL*8     GQBS  ( IJDIM,KMAX )    !! basic state q
      REAL*8     TTAU  ( IJDIM      )
      REAL*8     QTAU  ( IJDIM      )
      REAL*8     DQSAT ( IJDIM,KMAX )    !! change in qsat
      REAL*8     WTLSC ( IJDIM,KMAX )    !! weight for LSC
      REAL*8     TFLG  ( IJDIM,KMAX )    !! flag for Q1
      REAL*8     QFLG  ( IJDIM,KMAX )    !! flag for Q2

      SAVE       RHBS, GQBS
      SAVE       DQSAT, WTLSC
      SAVE       TTAU, QTAU
      SAVE       TFLG, QFLG
*
*   [INTERNAL WORK]
      REAL*8     GDP, GDPM, QSAT, DQF
      REAL*8     TTAUX ( IJDIM )
      REAL*8     QTAUX ( IJDIM )
      REAL*8     DQSATX( IJDIM,KMAX )
      REAL*8     WTLSCX( IJDIM,KMAX )
      REAL*8     TFLGX ( IJDIM,KMAX )
      REAL*8     QFLGX ( IJDIM,KMAX )
      REAL*8     DQ1, DQ2, DTD, DQD
      REAL*8     TTAU1, QTAU1

      INTEGER    IJ, K
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL PARM] 
      REAL*8     TTAUL0, QTAUL0 !! relaxation time due to LSC
      REAL*8     DSEC
      REAL*8     DQRAT
      REAL*8     FLND
      LOGICAL    OLSC           !! apply LSC?
      LOGICAL    OFIRST
      CHARACTER  TUNIT*4        !! unit of time
      NAMELIST  /NMLSC/  OLSC, TTAUL0, QTAUL0, TUNIT, DQRAT, FLND
*   [INTERNAL FUNC] 
#include        "zqsat.F"                /* saturate q */
      DATA       OLSC   / .TRUE. /
      DATA       TTAUL0 / 2.D0   /
      DATA       QTAUL0 / 2.D0   /
      DATA       TUNIT  / 'HOUR' /
      DATA       DSEC   / 0.D0   /
      DATA       DQRAT  / 1.D-3  /
      DATA       FLND   / 1.D0   /
      DATA       OFIRST / .TRUE. /
*
*===================================================================
      ENTRY SETLSC
     I         ( GFT   , GFQ   , GFPS , GIDX   )


      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ SETLSC: LARGE-SCALE COND. 02/08/14'
         OFIRST = .FALSE.
#ifdef OPT_POSDEF
         WRITE ( 6,* ) ' ### POSITIVE DEFINITE Q1/Q2 (NONLIN) '
#endif
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMLSC, END=190 )
  190    WRITE  ( JFPAR, NMLSC )

         CALL CXX2SS ( TTAU1, TTAUL0, TUNIT, DSEC  )
         CALL CXX2SS ( QTAU1, QTAUL0, TUNIT, DSEC  )
         TTAU1 = 1.D0 / TTAU1
         QTAU1 = 1.D0 / QTAU1
         CALL RESET( TTAU , IJDIM )
         CALL RESET( QTAU , IJDIM )
         IF( OLSC ) THEN
            DO 500 IJ = 1, IJDIM
               TTAU( IJ ) = TTAU1
               QTAU( IJ ) = QTAU1
               IF( GIDX( IJ ) .GT. 0.D0 ) THEN
                  TTAU( IJ ) = TTAU1 * FLND
                  QTAU( IJ ) = QTAU1 * FLND
               ENDIF
 500        CONTINUE
         ENDIF

         CALL COPY  ( GQBS , GFQ  , IJDIM*KMAX   )
         CALL SETV  ( TFLG , 1.D0 , IJDIM*KMAX   )
         CALL SETV  ( QFLG , 1.D0 , IJDIM*KMAX   )
*
*     dQs/dT and weight for LSC
*
         DO 800 K = 1, KMAX
            DO 800 IJ = 1, IJDIM
               GDPM  = GFPS( IJ ) * SIGM( K ) * 100.  
               QSAT  = FQSAT ( GFT( IJ,K ), GDPM         )
CCX               DQF   = GFQ( IJ,K ) - QSAT
               DQF   = ( GFQ( IJ,K ) - QSAT ) / DQRAT
               DQSAT( IJ,K ) = FDQSAT( GFT( IJ,K ), QSAT )
               WTLSC( IJ,K ) = 
     &              ( TANH( DQF ) + 1.D0 ) / 2.D0
 800     CONTINUE
*
*     BS relative humidity
*
         DO 900 K = 1, KMAX
            DO 900 IJ = 1, IJDIM
               GDP  = GFPS( IJ ) * SIG( K ) * 100.D0
               QSAT = FQSAT( GFT( IJ,K ), GDP )
               RHBS( IJ,K ) = GFQ( IJ,K ) / QSAT 
 900     CONTINUE 

CCX         call grdout
CCX     I     ( WTLSC ,  'alpha.grd' , 'ALP' , IDIM,JDIM,KMAX )
CCX         call xabort(1)

CCX         call grdout
CCX     I     ( RHBS ,  'rhbs.grd' , 'RH ' , IDIM,JDIM,KMAX )
CCX         call xabort(1)
*
      ENDIF
*
      RETURN
*===================================================================
      ENTRY    TQ2TQF_L         !! Ta,Qa -> Fh and Fq due to LSC
     O     ( GAFT , GAFQ , GAWT , GAWQ ,
     I       GAT  , GAQ    )
*
*     heat source & moisture sink (adding)
*
      CALL RESET( GAFT , IJDIM*KMAX  )
      CALL RESET( GAFQ , IJDIM*KMAX  )
      CALL RESET( GAWT , IJDIM*KMAX  )
      CALL RESET( GAWQ , IJDIM*KMAX  )

      DO 2000 K = 1, KMAX
         DO 2000 IJ = 1, IJDIM
            DQ1 = TTAU( IJ )*WTLSC( IJ,K )*GAQ( IJ,K )*EL / CP
            DQ2 = QTAU( IJ )*WTLSC( IJ,K )*GAT( IJ,K )*DQSAT( IJ,K ) 
            DTD = - TTAU( IJ )*WTLSC( IJ,K )*GAT( IJ,K )
     &            * DQSAT( IJ,K )*EL / CP
            DQD = - QTAU( IJ )*WTLSC( IJ,K )*GAQ( IJ,K )
#ifdef OPT_POSDEF
CCX            IF( GAQ(IJ,K)+GFQ(IJ,K) .GT. 0.D0   ) THEN
CCX            IF( DQ1+DTD             .GT. 0.D0        ) THEN
            IF( GAQ(IJ,K)+GFQ(IJ,K) .GT. 0.D0 .AND.
     &          DQ1+DTD             .GT. 0.D0        ) THEN
#endif
            GAWT( IJ,K ) = DQ1
            GAWQ( IJ,K ) = DQ2
            GAFT( IJ,K ) = DTD
            GAFQ( IJ,K ) = DQD
#ifdef OPT_POSDEF
               TFLG( IJ,K ) = 1.D0
               QFLG( IJ,K ) = 1.D0
            ELSE
               TFLG( IJ,K ) = 0.D0
               QFLG( IJ,K ) = 0.D0
            ENDIF
#endif
 2000 CONTINUE
*
*
      RETURN
*===================================================================
      ENTRY GETLSCT
     O         ( TTAUX , QTAUX , DQSATX , WTLSCX  )
*
      IF( OFIRST ) THEN
         WRITE ( 6,* ) ' ### ERROR: SETLSC MUST BE CALLED FIRST ### '
         CALL XABORT( 1 )
      ENDIF
*
      DO 2500 IJ = 1, IJDIM
         TTAUX( IJ ) = TTAU( IJ )
         QTAUX( IJ ) = QTAU( IJ )
 2500 CONTINUE
*
      DO 2600 K = 1, KMAX
         DO 2600 IJ = 1, IJDIM
            DQSATX  ( IJ,K ) = DQSAT( IJ,K )
            WTLSCX  ( IJ,K ) = WTLSC( IJ,K )
 2600 CONTINUE
*
      RETURN
*===================================================================
      ENTRY GETFLGL
     O         ( TFLGX , QFLGX  )
*
      IF( OFIRST ) THEN
         WRITE ( 6,* ) ' ### ERROR: SETLSC MUST BE CALLED FIRST ### '
         CALL XABORT( 1 )
      ENDIF

      DO 2700 K = 1, KMAX
         DO 2700 IJ = 1, IJDIM
            TFLGX( IJ,K ) = TFLG( IJ,K )
            QFLGX( IJ,K ) = QFLG( IJ,K )
 2700 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE Q2P 
     O     ( GPR ,
     I       GQC , GQL , GPS )
*
*     convert Q1 to precip.
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     GPR ( IJDIM        ) !! precipitation
*
*   [INPUT] 
      REAL*8     GQC ( IJDIM,KMAX   ) !! Q1 from convection
      REAL*8     GQL ( IJDIM,KMAX   ) !! Q1 from lsc cloud
      REAL*8     GPS ( IJDIM        ) !! BS surface pressure
*
*   [WORK] 
      INTEGER    IJ, K
*
*   [INTERNAL PARAM] 
      LOGICAL    OFIRST
      REAL*8     QINT
      REAL*8     S2D
*
      DATA       OFIRST / .TRUE. /
      DATA       S2D    / 86400.D0 /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ Q2P: PRECIPITATION DIAG 03/09/02'
         OFIRST = .FALSE.
      ENDIF
*
      DO 1000 IJ = 1, IJDIM
         QINT = 0.D0
         DO 1100 K = 1, KMAX
            QINT = QINT + ( GQC( IJ,K ) + GQL( IJ,K ) )*DSIG( K )
 1100    CONTINUE
         GPR( IJ ) = CP * GPS( IJ ) * QINT * S2D * 1.D2
     &                              / ( EL * GRAV )
 1000 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE STDOUT         !!  draw data on a map
     I     ( XDAT ,  CVAR , IDIMD , JDIMD , KDIMD ,  FACT  )
*
*   [INPUT] 
      INTEGER    IDIMD, JDIMD, KDIMD
      REAL*8     FACT
      REAL*8     XDAT ( IDIMD,JDIMD,KDIMD   ) !! data
      CHARACTER  CVAR*4
      CHARACTER  CFIL*(*)
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
      ENTRY GRDOUT         !!  write down data to GrADS file
     I     ( XDAT ,  CFIL , CVAR , IDIMD , JDIMD, KDIMD  )
*
*
      WRITE ( 6,* ) ' *** GRDOUT FOR: ', CVAR

      OPEN( 91 , FILE=CFIL , FORM='UNFORMATTED' )
      DO K = 1, KDIMD
         WRITE( 91 )
     &        ( ( SNGL( XDAT(I,J,K) ),I = 1, IDIMD-1 ),J = 1, JDIMD ) 
      ENDDO
      CLOSE( 91 )
*
      RETURN
      END

