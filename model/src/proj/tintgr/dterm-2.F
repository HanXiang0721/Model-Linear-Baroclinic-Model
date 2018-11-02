* PACKAGE DCIRC !! dynamics: terms of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         01/07/10(hiro)     linear MCA after Neelin&Yu (1994,JAS)
*         02/08/15(hiro)     linear LSC
**********************************************************************
      SUBROUTINE DYNTRM    !! dynamics terms
     O         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     O           GSIGD , GPID  , GAVOR , GADIV , GATV  ,
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           TFSEND, FSFRC , OFRC  , OSSTF , NSFCS , OVD   ,
     M           TIME  ,
     M           GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA,
     W           GTUX  , GTVX  , GTXX  , WTKE  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX      )  !! tendency of humidity
*
      REAL*8     GSIGD ( IJDIM, KMAX+1     )  !! tendency of sigma
      REAL*8     GPID  ( IJDIM             )  !! tendency of pai
      REAL*8     GAVOR ( IJDIM, KMAX       )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX       )  !! divergence D 
      REAL*8     GATV  ( IJDIM, KMAX       )  !! virtulal tmp.
*
*   [INPUT] 
      REAL*8     GFU   ( IJDIM, KMAX      )  !! BS westerly u
      REAL*8     GFV   ( IJDIM, KMAX      )  !! BS southery wind v
      REAL*8     GFT   ( IJDIM, KMAX      )  !! BS temperature
      REAL*8     GFPS  ( IJDIM            )  !! BS surface pressure
      REAL*8     GFQ   ( IJDIM, KMAX      )  !! BS humidity
*
      REAL*8     GBU   ( IJDIM, KMAX       )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX       )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX       )  !! temperature T
      REAL*8     GBPS  ( IJDIM             )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX       )  !! humidity
*
      REAL*8     SALPHA( KMAX  )              !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX  )              !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX  )              !! fact. of vert. intrp. T
      REAL*8     ATF   ( KMAX  )              !! fact. of vert. intrp. T
      REAL*8     BTF   ( KMAX  )              !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX  )              !! basic temp. (full lev)
      REAL*8     TMTX  ( KMAX, KMAX )         !! for semi-implicit
* 
      REAL*8     CORIOL( IJDIM )              !! Corioris factor f
      REAL*8     FLAPLA( NMDIM )              !! Laplacian factor
*
      REAL*8     TIME                         !! time
      REAL*8     TFSEND                       !! finish t steady forcing
      CHARACTER  FSFRC * 70                   !! filename steady forcing
      LOGICAL    OFRC                         !! add steady forcing ?
      LOGICAL    OSSTF                        !! is forcing SST?
      LOGICAL    OVD                          !! initial pert. = vor,div
      INTEGER    NSFCS                        !! no. case steady forcing
*
*   [MODIFY]
      REAL*8     GAU   ( IJDIM, KMAX       )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX       )  !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX       )  !! temperature T
      REAL*8     GAPS  ( IJDIM             )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX       )  !! humidity
*
*   [WORK] 
      REAL*8     GBVOR ( IJDIM, KMAX       )  !! vorticity
      REAL*8     GBDIV ( IJDIM, KMAX       )  !! divergence D 
      REAL*8     GAFT  ( IJDIM, KMAX   )  !! forcing for temp., f(T')
      REAL*8     GAFQ  ( IJDIM, KMAX   )  !! forcing for Q, f(Q')
      REAL*8     GAWT  ( IJDIM, KMAX   )  !! forcing for temp., f(W')
      REAL*8     GAWQ  ( IJDIM, KMAX   )  !! forcing for Q, f(W')
      REAL*8     GAFTL ( IJDIM, KMAX   )  !! forcing for temp., f(T')
      REAL*8     GAFQL ( IJDIM, KMAX   )  !! forcing for Q, f(Q')
      REAL*8     GAWTL ( IJDIM, KMAX   )  !! forcing for temp., f(W')
      REAL*8     GAWQL ( IJDIM, KMAX   )  !! forcing for Q, f(W')
      REAL*8     GAPADV( IJDIM, KMAX   )  !! advection of pai
      REAL*8     GSUMP ( IJDIM, KMAX   )  !! vert. integ. of tend. of pi
      REAL*8     GTUX  ( IJDIM, KMAX   )  !! tend. zonal  adv.
      REAL*8     GTVX  ( IJDIM, KMAX   )  !! tend. merid. adv.
      REAL*8     GTXX  ( IJDIM, KMAX   )  !! tend.
      REAL*8     GFTXX ( IJDIM, KMAX   )  !! tend.
      REAL*8     GWX   ( IJDIM, KMAX   )  !! work
      REAL*8     GWY   ( IJDIM, KMAX   )  !! work
      REAL*8     GAPIX ( IJDIM         )  !! zonal deriv. of pi
      REAL*8     GAPIY ( IJDIM         )  !! merid. differ. of pi
      REAL*8     GAW   ( IJDIM, KMAX   )  !! omega
      REAL*8     GPRC  ( IJDIM         )  !! precipitation [mm/dy]
      REAL*8     WTKE  ( NMDIM, KMAX   )  !! KE term
      REAL*8     FACA, PADVA, PADVF
      INTEGER    IJ, K, L, NM
*
*   [INTERNAL SAVE] !! variables for basic state
      REAL*8     GFVOR( IJDIM, KMAX )    !! BS vorticity
      REAL*8     GFDIV( IJDIM, KMAX )    !! BS divergence
      REAL*8     GFSIGD( IJDIM, KMAX+1)  !! BS sigma velocity
      REAL*8     GFPID ( IJDIM        )  !! BS tend. of pai
      REAL*8     GFTV  ( IJDIM, KMAX  )  !! BS virtulal tmp.
      REAL*8     GFPADV( IJDIM, KMAX )   !! advection of pai 
      REAL*8     GFSUMP( IJDIM, KMAX )   !! vert. integ. tend. pi
      REAL*8     GFPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GFPIY ( IJDIM       )   !! merid. differ. of pi 
*
      REAL*8     GSFV ( IJDIM, KMAX )    !! steady forcing for vor.
      REAL*8     GSFD ( IJDIM, KMAX )    !! steady forcing for div.
      REAL*8     GSFT ( IJDIM, KMAX )    !! steady forcing for temp.
      REAL*8     GSFP ( IJDIM       )    !! steady forcing for LnPs
      REAL*8     GSFQ ( IJDIM, KMAX )    !! steady forcing for humidity
*
      REAL*8     GDMPV( IJDIM, KMAX )    !! vor. damping coeff
      REAL*8     GDMPD( IJDIM, KMAX )    !! div. damping coeff
      REAL*8     GDMPT( IJDIM, KMAX )    !! tmp. damping coeff
      REAL*8     GDMPQ( IJDIM, KMAX )    !! sph  damping coeff
*
      SAVE       GFVOR, GFDIV, GFSIGD
      SAVE       GFPID, GFTV, GFPADV, GFSUMP, GFPIX, GFPIY
      SAVE       GSFV, GSFD, GSFT, GSFP, GSFQ
      SAVE       GDMPV, GDMPD, GDMPT, GDMPQ
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
*   [INTERNAL PARAM.] 
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DTERM: DYNAMICS EQ. TERMS 98/11/12'
*
*     additional damping over land
*
         CALL GETLDMP ( GDMPV , GDMPD , GDMPT , GDMPQ )
*
*     steady forcing
*
         CALL RESET( GSFV   , IJDIM*KMAX )
         CALL RESET( GSFD   , IJDIM*KMAX )
         CALL RESET( GSFT   , IJDIM*KMAX )
         CALL RESET( GSFP   , IJDIM      )
         CALL RESET( GSFQ   , IJDIM*KMAX )
         CALL RESET( GAFT   , IJDIM*KMAX )
         CALL RESET( GAFTL  , IJDIM*KMAX )
         CALL RESET( GAFQ   , IJDIM*KMAX )
         CALL RESET( GAFQL  , IJDIM*KMAX )
         CALL RESET( GAWT   , IJDIM*KMAX )
         CALL RESET( GAWTL  , IJDIM*KMAX )
         CALL RESET( GAWQ   , IJDIM*KMAX )
         CALL RESET( GAWQL  , IJDIM*KMAX )

         CALL RDSFRC
     O         ( GSFV  , GSFD  , GSFT  , GSFP  , GSFQ  ,
     I           OFRC  , OSSTF , FSFRC , NSFCS  )
*
*     convert initial vor. div. to u v
*
         IF( OVD ) THEN
            CALL COPY  ( GAVOR, GAU, IJDIM*KMAX )
            CALL COPY  ( GADIV, GAV, IJDIM*KMAX )
            CALL RESET ( GAU  ,      IJDIM*KMAX )
            CALL RESET ( GAV  ,      IJDIM*KMAX )
            CALL VD2UVG
     O           ( GAU   , GAV   ,
     I             GAVOR , GADIV , .TRUE., .TRUE.,
     W             WTVOR , WTDIV  )
         ELSE
            CALL UV2VDG
     O           ( GAVOR , GADIV ,
     I             GAU   , GAV   , .TRUE., .TRUE.,
     W             GTUX  , GTVX  , WTVOR , WTDIV  )
         ENDIF
*
*     calculation for basic state
*
         CALL UV2VDG
     O        ( GFVOR , GFDIV ,
     I        GFU   , GFV   , .TRUE., .TRUE.,
     W        GTUX  , GTVX  , WTVOR , WTDIV  )
*
         DO 1100 IJ = 1, IJDIM
            GFTXX( IJ,1 )  = LOG( GFPS( IJ ) )
 1100    CONTINUE 
         CALL HGRAD             !! pi gradient
     O        ( GFPIX , GFPIY ,
     I        GFTXX   , 1     ,
     W        WTKE           )
*
         CALL PSDOTB            !!  surface pres. change & sigma velocity
     O         ( GFSIGD, GFSUMP, GFPADV, GFPID ,
     I           GFU   , GFV   , GFDIV , GFPIX , GFPIY  )
*
         CALL VIRTMB
     O        ( GFTV  ,
     I          GFT   , GFQ     )
*
         OFIRST = .FALSE.
      ENDIF
*
*     reset steady forcing?
*     
      IF( TFSEND .GT. 0.D0 .AND. TIME .GT. TFSEND ) THEN
         WRITE ( 6,* ) ' @@@ RESET STEADY FORCING'
         CALL RESET ( GSFV  ,      IJDIM*KMAX )
         CALL RESET ( GSFD  ,      IJDIM*KMAX )
         CALL RESET ( GSFT  ,      IJDIM*KMAX )
         CALL RESET ( GSFP  ,      IJDIM      )
         CALL RESET ( GSFQ  ,      IJDIM*KMAX )
         TFSEND = -999
      ENDIF
*
*     internal forcing due to surface T, Q fluxes
*
#ifndef OPT_CLASSIC
      CALL UV2TQF               !! sfc. flux (Ta,Qa,Va -> Fh,Fq)
     O     ( GAFT , GAFQ , GAWT , GAWQ ,
     I       GAU  , GAV  , GAT  , GAQ    )
      CALL TQ2TQF               !! MCA (Ta,Qa -> Fh,Fq)
     M     ( GAFT , GAFQ , GAWT , GAWQ ,
     I       GAT  , GAQ                  )
      CALL TQ2TQF_L             !! LSC (Ta,Qa -> Fh,Fq)
     M     ( GAFTL, GAFQL, GAWTL, GAWQL,
     I       GAT  , GAQ    )
#endif
*
*    << circulations >>
*
      CALL UV2VDG               !! UV-->VOR.DIV.
     O     ( GAVOR , GADIV ,
     I     GAU   , GAV   , .TRUE., .TRUE.,
     W     GTUX  , GTVX  , WTVOR , WTDIV  )

      CALL UV2VDG               !! UV-->VOR.DIV.
     O     ( GBVOR , GBDIV ,
     I     GBU   , GBV   , .TRUE., .TRUE.,
     W     GTUX  , GTVX  , WTVOR , WTDIV  )
*
*    << pressure eq. >>
*
      DO 1200 IJ = 1, IJDIM
         GTXX( IJ,1 ) = LOG( GAPS(IJ)+GFPS(IJ) ) - LOG( GFPS( IJ ) )
 1200 CONTINUE 
      CALL HGRAD                       !! perturbation pi gradient
     O         ( GAPIX , GAPIY ,
     I           GTXX , 1     ,
     W           WTKE           )
*
      CALL PSDOT     !!  surface pres. change & sigma velocity
     O         ( GSIGD , GSUMP , GAPADV, GPID  ,
     I           GAU   , GAV   , GADIV , GAPIX , GAPIY , 
     I           GFU   , GFV   , GFPIX , GFPIY  )
*
      CALL COPY ( GTXX, GPID, IJDIM )
      DO 2100 IJ = 1, IJDIM
         DO 2200 K = 1, KMAX
            GTXX( IJ,1 )  = GTXX( IJ,1 )  + DSIG( K )*GADIV( IJ,K )
 2200    CONTINUE
*
*     forcing to surface pressure
*
         GTXX( IJ, 1) = GTXX( IJ, 1) + GSFP( IJ ) 
 2100 CONTINUE 
*
      CALL G2W
     O         ( WTPI  ,
     I           GTXX  ,
     I           '    ', 'POSO',  1  )
*
*    << temperature eq. >>
*
      CALL VIRTMP
     O         ( GATV  ,
     I           GFT   , GFQ   , GAT   , GAQ     )
*
      CALL GRTADV                      !!  temperature advection
     O         ( GTUX  , GTVX  , GTXX  ,
     I           GAT   , GATV  ,
     I           GFT   , GFTV  ,
     I           GAU   , GAV   , GADIV , 
     I           GFU   , GFV   , GFDIV , 
     I           GSIGD , GSUMP , GAPADV,
     I           GFSIGD, GFSUMP, GFPADV,
     C           SALPHA, SBETA , SKAPPA,
     C           ATF   , BTF   , TBAR    )
*
      DO 3100 L = 1, KMAX
         DO 3100 K = 1, KMAX
            DO 3100 IJ = 1, IJDIM
               GTXX( IJ,K )  = GTXX( IJ,K ) + TMTX( K,L )*GADIV( IJ,L )
 3100 CONTINUE 
*
*     forcing to temperature
*
      DO 3200 K = 1, KMAX
         DO 3200 IJ = 1, IJDIM
            GTXX( IJ,K ) = GTXX( IJ,K ) + GSFT( IJ, K) 
     &                   + GAWT( IJ,K ) + GAWTL( IJ,K )
     &                   - GDMPT( IJ,K ) * GBT( IJ,K )
 3200 CONTINUE 
*
      CALL HCONVW
     O         ( WTT   ,
     M           GTUX  , GTVX  , 
     I           KMAX            )
      CALL G2W
     M         ( WTT   ,
     I           GTXX  , '    ', 'ADDO', KMAX   )
*
*    << momentum eq. >>
*
      CALL GRUADV                      !!  momentum advection
     O         ( GTUX  , GTVX  , GTXX  ,
     I           GAU   , GAV   , GAVOR , 
     I           GFU   , GFV   , GFVOR , 
     I           GAPIX , GAPIY , GSIGD , GATV  , GAT   ,
     I           GFPIX , GFPIY , GFSIGD, GFTV  , 
     C           CORIOL, 
     C           SALPHA, SBETA , SKAPPA, TBAR           )
*
      CALL       UV2VDW    !! (U,V) -> (vor.,D)spectral
     O         ( WTVOR , WTDIV ,
     I           GTUX  , GTVX  , .TRUE., .TRUE.,
     W           GWX   , GWY                     )
*
      CALL G2W
     M         ( WTVOR ,
     I           GSFV  , '    ', 'ADDO', KMAX   )
      CALL G2W
     M         ( WTDIV ,
     I           GSFD  , '    ', 'ADDO', KMAX   )
*
*     damping over land
*
      DO 3300 K = 1, KMAX
         DO 3300 IJ = 1, IJDIM
            GTUX( IJ,K ) = - GDMPV( IJ,K ) * GBVOR( IJ,K )
            GTVX( IJ,K ) = - GDMPD( IJ,K ) * GBDIV( IJ,K )
 3300 CONTINUE 
      CALL G2W
     M         ( WTVOR ,
     I           GTUX  , '    ', 'ADDO', KMAX   )
      CALL G2W
     M         ( WTDIV ,
     I           GTVX  , '    ', 'ADDO', KMAX   )
*
      CALL G2W
     O         ( WTKE  ,
     I           GTXX  ,
     I           '    ', 'POSO', KMAX )
*
      DO 4100 K = 1 , KMAX
         DO 4100 NM = 1 , NMDIM
            WTDIV ( NM,K ) =  WTDIV ( NM,K ) 
     &                     -  WTKE  ( NM,K ) * FLAPLA( NM )
 4100 CONTINUE
*
*    << moisture eq. >>
*
      CALL GRQADV                      !!  adv. of humidity etc.
     O         ( GTUX  , GTVX  , GTXX  ,
     I           GAQ   , GFQ   ,
     I           GAU   , GAV   , GADIV , GSIGD , 
     I           GFU   , GFV   , GFDIV , GFSIGD    )
*
*     forcing to humidity
*
      DO 4200 K = 1, KMAX
         DO 4200 IJ = 1, IJDIM
            GTXX( IJ,K ) = GTXX( IJ,K ) + GSFQ( IJ, K) 
     &                   + GAWQ( IJ,K ) + GAWQL( IJ, K) 
     &                   - GDMPQ( IJ,K ) * GBQ( IJ,K )
 4200 CONTINUE 
*
      CALL HCONVW
     O         ( WTQ   ,
     M           GTUX  , GTVX  , 
     I           KMAX            )
*
      CALL G2W
     M         ( WTQ   ,
     I           GTXX  , '    ', 'ADDO', KMAX )
*
*    << diagnostic for omega >>
*
      FACA = SIG( KMAX ) - SIGM( KMAX+1 )
      DO 5000 IJ = 1, IJDIM
         PADVA = ( GADIV( IJ,KMAX ) + GAPADV( IJ,KMAX ) ) * FACA
         PADVF = ( GFDIV( IJ,KMAX ) + GFPADV( IJ,KMAX ) ) * FACA
         GAW( IJ,KMAX ) = 
     &        ( SIG( KMAX )*GAPADV( IJ,KMAX ) - PADVA ) * GFPS( IJ ) 
     &      + ( SIG( KMAX )*GFPADV( IJ,KMAX ) - PADVF ) * GAPS( IJ ) 
 5000 CONTINUE
      DO 5100 K = 1, KMAX-1
         DO 5100 IJ = 1, IJDIM
            FACA = SIG( K ) - SIGM( K+1 )
            PADVA = ( GADIV( IJ,K ) + GAPADV( IJ,K ) ) * FACA
            PADVF = ( GFDIV( IJ,K ) + GFPADV( IJ,K ) ) * FACA
            GAW( IJ,K ) = 
     &           ( SIG(K)*GAPADV( IJ,K )-(GSUMP( IJ,K+1 )+PADVA) )
     &           * GFPS( IJ )
     &         + ( SIG(K)*GFPADV( IJ,K )-(GFSUMP( IJ,K+1 )+PADVF) )
     &           * GAPS( IJ )
 5100 CONTINUE
      CALL HISTIN
     I   ( GAW,'OMGF','p-vel full level','hPa/s','ALEV' )
*
#ifndef OPT_CLASSIC
*
*    << history for precipitation >>
*
      DO 6000 K = 1, KMAX
         DO 6000 IJ = 1, IJDIM
            GAFT ( IJ,K ) = GAFT( IJ,K )  + GAWT( IJ,K ) 
            GAFQ ( IJ,K ) = GAFQ( IJ,K )  + GAWQ( IJ,K ) 
            GAFTL( IJ,K ) = GAFTL( IJ,K ) + GAWTL( IJ,K ) 
            GAFQL( IJ,K ) = GAFQL( IJ,K ) + GAWQL( IJ,K ) 
 6000 CONTINUE
      CALL Q2P 
     O     ( GPRC ,
     I       GAFT , GAFTL, GFPS )
*
#ifdef OPT_OUTPOSDEF
      DO 6050 IJ = 1, IJDIM
         IF( GPRC( IJ ) .LT. 0.D0 ) THEN
            DO 6080 K = 1, KMAX
               GAFT ( IJ,K ) = 0.D0
               GAFTL( IJ,K ) = 0.D0
               GAFQ ( IJ,K ) = 0.D0
               GAFQL( IJ,K ) = 0.D0
 6080       CONTINUE
            GPRC( IJ ) = 0.D0
         ENDIF
 6050 CONTINUE
#endif
*
      CALL HISTIN
     I   ( GPRC, 'PR','precipitation','mm/dy','ASFC')
*
*    << history output for Q1, Q2 >>
*
      DO 6100 K = 1, KMAX
         DO 6100 IJ = 1, IJDIM
            GAFT ( IJ,K ) = GAFT( IJ,K ) + GSFT( IJ,K ) 
            GAFQ ( IJ,K ) = GAFQ( IJ,K ) + GSFQ( IJ,K ) 
 6100 CONTINUE
*
      CALL HISTIN
     I   ( GAFT, 'Q1C','convective heat source','K/s','ALEV')
      CALL HISTIN
     I   ( GAFQ, 'Q2C','convective moisture source','kg/kg/s','ALEV')
      CALL HISTIN
     I   ( GAFTL,'Q1L','LSC heat source','K/s','ALEV')
      CALL HISTIN
     I   ( GAFQL,'Q2L','LSC moisture source','kg/kg/s','ALEV')
#endif
*
      RETURN
      END
***********************************************************************
      SUBROUTINE VIRTMB    !! basic state virtual temperature
     O         ( GFTV  ,
     I           GFT   , GFQ     )
*
*   [PARAM] 
#include        "zcdim.F"
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"
*
*   [OUTPUT]
      REAL*8     GFTV  ( IJDIM, KMAX )        !! virtual temperature
*
*   [INPUT]
      REAL*8     GFT   ( IJDIM, KMAX       )  !! temperature T
      REAL*8     GFQ   ( IJDIM, KMAX       )  !! humidity 
*
*   [ENTRY OUTPUT]
      REAL*8     GATV  ( IJDIM, KMAX )        !! virtual temperature
*
*   [ENTRY INPUT]
      REAL*8     GAT   ( IJDIM, KMAX       )  !! temperature T
      REAL*8     GAQ   ( IJDIM, KMAX       )  !! humidity 
*
*   [INTERNAL WORK]
      INTEGER    IJ, K
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ VIRTMB: VERTUAL TEMP. 01/06/15'
         OFIRST = .FALSE.
      ENDIF
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GFTV( IJ,K ) = GFT( IJ,K )
     &                   * ( 1.+ EPSVT*GFQ( IJ,K ) )
 1100 CONTINUE
*
      RETURN
*=======================================================================
      ENTRY VIRTMP         !! perturbation virtual temperature
     O         ( GATV  ,
     I           GFT   , GFQ    , 
     I           GAT   , GAQ     )
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ VIRTMP: VERTUAL TEMP. 01/06/15'
         OFIRST = .FALSE.
      ENDIF
*
      DO 1200 K = 1, KMAX
         DO 1200 IJ = 1, IJDIM
            GATV( IJ,K ) = GAT( IJ,K ) + EPSVT * 
     &                   ( GAT( IJ,K ) * GFQ( IJ,K ) 
     &                   + GFT( IJ,K ) * GAQ( IJ,K ) )
 1200 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE RDSFRC    !!  get steady forcing
     O         ( GSFV  , GSFD  , GSFT  , GSFP  , GSFQ  ,
     I           OFRC  , OSSTF , FSFRC , NSFCS  )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     GSFV ( IJDIM, KMAX )    !! steady forcing for vor.
      REAL*8     GSFD ( IJDIM, KMAX )    !! steady forcing for div.
      REAL*8     GSFT ( IJDIM, KMAX )    !! steady forcing for temp.
      REAL*8     GSFP ( IJDIM       )    !! steady forcing for LnPs
      REAL*8     GSFQ ( IJDIM, KMAX )    !! steady forcing for humidity
*
*   [INPUT]
      CHARACTER  FSFRC * 70              !! filename steady forcing
      LOGICAL    OFRC                    !! add steady forcing ?
      LOGICAL    OSSTF                   !! is forcing SST?
      INTEGER    NSFCS                   !! no. case steady forcing
*
*   [WORK]
#ifndef OPT_CLASSIC
      REAL*8     GSST ( IJDIM )          !! steady SST forcing
      REAL*8     GWG  ( IJDIM )          !! steady soil wetness forcing
#endif
      REAL*4     X     ( IMAX, JMAX    ) !! temporary for forcing data
      INTEGER    K, IJ, I, J
*
      IF( OFRC ) THEN

         OPEN( 88, FILE = FSFRC, FORM='UNFORMATTED',
     &        STATUS='UNKNOWN' ) 
         WRITE( 6, *) '    $$$ STEADY FORCING FILE:', FSFRC,' $$$'
*     
*     read forcing data
*
#ifdef OPT_CLASSIC
         K = ( NSFCS-1 ) * ( 3*KMAX+1 )
#else
         K = ( NSFCS-1 ) * ( 4*KMAX+1 )
         IF( .NOT. OSSTF ) THEN !! forcing is not SST
#endif
         IF( K .GT. 0 ) THEN
            DO 90 I = 1, K
               READ( 88 ) X     !! dummy
 90         CONTINUE
         ENDIF
         DO 100 K = 1, KMAX
            READ( 88 ) X        !! vorticity
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
            READ( 88 ) X        !! divergence
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
            READ( 88 ) X        !! temperature
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
         READ( 88 ) X           !! Ps
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
#ifndef OPT_CLASSIC
         DO 210 K = 1, KMAX
            READ( 88 ) X        !! specific humidity
            IJ = 0
            DO 220 J = 1, JMAX
               DO 230 I = 1, IMAX
                  IJ = IJ + 1
                  GSFQ( IJ, K) = DBLE( X( I, J) )
 230           CONTINUE
               IJ = IJ + 1
               GSFQ( IJ, K) = DBLE( X( 1, J) )
 220        CONTINUE
 210     CONTINUE
         write(6,*) 'sph.'
*
         ENDIF
*     
#endif
*
*
#ifndef OPT_CLASSIC
         IF( .NOT. OSSTF ) GOTO 999 !! when forcing is not SST
*
         K = ( NSFCS-1 ) * 2
         IF( K .GT. 0 ) THEN
            DO 300 I = 1, K
               READ( 88 ) X     !! dummy
 300        CONTINUE
         ENDIF
         READ( 88 ) X           !! SST anomaly
         IJ = 0
         DO 310 J = 1, JMAX
            DO 320 I = 1, IMAX
               IJ = IJ + 1
               GSST( IJ ) = DBLE( X( I, J) )
 320        CONTINUE
            IJ = IJ + 1
            GSST( IJ ) = DBLE( X( 1, J) )
 310     CONTINUE
         write(6,*) 'sst anom.'
*     
         READ( 88 ) X           !! soil wetness
         IJ = 0
         DO 330 J = 1, JMAX
            DO 340 I = 1, IMAX
               IJ = IJ + 1
               GWG( IJ ) = DBLE( X( I, J) )
 340        CONTINUE
            IJ = IJ + 1
            GWG( IJ ) = DBLE( X( 1, J) )
 330     CONTINUE
         write(6,*) 'soil wetness anom.'
*     
         CALL TS2TQF
     O        ( GSFT , GSFQ ,
     I          GSST           )
*
         CALL WG2TQF
     M        ( GSFT , GSFQ ,
     I          GWG            )
*
#endif
 999     CLOSE( 88 )

      ENDIF


      RETURN
      END
**********************************************************************
      SUBROUTINE RDIFRC         !!  get initial perturbation
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     M           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           OPER  , FFRC  , NFCS   )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
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
*
*   [INPUT]
      CHARACTER  FFRC * 70               !! filename initial anomaly
      LOGICAL    OPER                    !! read initial perturbations?
      INTEGER    NFCS                   !! no. case steady forcing
*
*   [WORK]
      REAL*4     X     ( IMAX, JMAX    ) !! temporary for forcing data
      INTEGER    K, IJ, I, J, MVAR
*
      CALL RESET( GAU   , IJDIM*KMAX )
      CALL RESET( GAV   , IJDIM*KMAX )
      CALL RESET( GAT   , IJDIM*KMAX )
      CALL RESET( GAPS  , IJDIM      )
      CALL RESET( GAQ   , IJDIM*KMAX )
*
      IF( OPER ) THEN

         OPEN( 77, FILE = FFRC, FORM='UNFORMATTED', 
     &        STATUS='UNKNOWN' )
         WRITE( 6, *) '    $$$ PERTURBATION FILE:', FFRC,' $$$'
*     
*     read perturbation data
*
#ifdef OPT_CLASSIC
         MVAR = 3
#else
         MVAR = 4
#endif
         K = ( NFCS - 1 ) * ( MVAR * KMAX + 1 )
         IF( K .GT. 0 ) THEN
            DO 90 I = 1, K
               READ( 77 ) X     !! dummy
 90         CONTINUE
         ENDIF
         DO 100 K = 1, KMAX
            READ( 77 ) X        !! zonal wind
            IJ = 0
            DO 110 J = 1, JMAX
               DO 120 I = 1, IMAX
                  IJ = IJ + 1
                  GAU( IJ, K) = DBLE( X( I, J) )
 120           CONTINUE
               IJ = IJ + 1
               GAU( IJ, K) = DBLE( X( 1, J) )
 110        CONTINUE
 100     CONTINUE
*     
         DO 130 K = 1, KMAX
            READ( 77 ) X        !! meridional wind
            IJ = 0
            DO 140 J = 1, JMAX
               DO 150 I = 1, IMAX
                  IJ = IJ + 1
                  GAV( IJ, K) = DBLE( X( I, J) )
 150           CONTINUE
               IJ = IJ + 1
               GAV( IJ, K) = DBLE( X( 1, J) )
 140        CONTINUE
 130     CONTINUE
*     
         DO 160 K = 1, KMAX
            READ( 77 ) X        !! temperature
            IJ = 0
            DO 170 J = 1, JMAX
               DO 180 I = 1, IMAX
                  IJ = IJ + 1
                  GAT( IJ, K) = DBLE( X( I, J) )
 180           CONTINUE
               IJ = IJ + 1
               GAT( IJ, K) = DBLE( X( 1, J) )
 170        CONTINUE
 160     CONTINUE
*     
         READ( 77 ) X           !! Ps
         IJ = 0
         DO 190 J = 1, JMAX
            DO 200 I = 1, IMAX
               IJ = IJ + 1
               GAPS( IJ   ) = DBLE( X( I, J) )
 200        CONTINUE
            IJ = IJ + 1
            GAPS( IJ   ) = DBLE( X( 1, J) )
 190     CONTINUE
*     
#ifndef OPT_CLASSIC
         DO 210 K = 1, KMAX
            READ( 77 ) X        !! humidity
            IJ = 0
            DO 220 J = 1, JMAX
               DO 230 I = 1, IMAX
                  IJ = IJ + 1
                  GAQ( IJ, K) = DBLE( X( I, J) )
 230           CONTINUE
               IJ = IJ + 1
               GAQ( IJ, K) = DBLE( X( 1, J) )
 220        CONTINUE
 210     CONTINUE     
#endif
         CLOSE( 77 )
      ENDIF
*
      CALL COPY( GBU  , GAU  , IJDIM*KMAX     )
      CALL COPY( GBV  , GAV  , IJDIM*KMAX     )
      CALL COPY( GBT  , GAT  , IJDIM*KMAX     )
      CALL COPY( GBPS , GAPS , IJDIM          )
      CALL COPY( GBQ  , GAQ  , IJDIM*KMAX     )
*     
      RETURN
      END
**********************************************************************
      SUBROUTINE SPECSM    !!  remove highest components
     M         ( GX  ,  KMAXD ,
     I           LCUT,
     W           WX              )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
*
*   [INPUT]
      INTEGER    KMAXD
      INTEGER    LCUT
      CHARACTER  CSMD*(*)
*
*   [MODIFY]
      REAL*8     GX ( IDIM,JDIM,KMAXD )    !!
*
*   [SAVE]
      INTEGER    NMO   ( 2, 0:NMAX, 0:NMAX ) !! order of spect. suffix
      SAVE       NMO
*
*   [INTERNAL WORK]
      REAL*8     GY ( IDIM,JDIM,KMAXD )    !!
      REAL*8     WX( NMDIM, KMAXD)
      REAL*8     W0, W1
      INTEGER    I, J, K, L, M, LEND
      INTEGER    I1, I2, J1, J2
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /

      IF( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL DSETNM
     O         ( NMO   )
      ENDIF
*
      CALL G2W
     O     ( WX  ,
     I       GX  , '    ', 'POS ', KMAXD )
*
      DO 1000 K = 1, KMAXD
         DO 1000 M = 0 , MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
            DO 1100 L = 0 , LEND
               IF ( L .GT. LCUT ) THEN
                  WX( NMO(1,M,L),K )  = 0.D0
                  WX( NMO(2,M,L),K )  = 0.D0
               ENDIF
 1100       CONTINUE
 1000 CONTINUE
*     
      CALL W2G
     O     ( GX  ,
     I       WX  , '    ', 'POSO', KMAXD )
*
      RETURN
*=======================================================================
      ENTRY GRIDSM         !! smoothing on grid
     O         ( GX  , KMAXD , CSMD ,
     W           GY                   )

      CALL COPY( GY , GX , IDIM*JDIM*KMAXD)
      W0 = 0.7D0
      W1 = ( 1.D0 - W0 ) / 2.D0
      DO 1500 K = 1, KMAXD
         IF( CSMD(1:1) .EQ. 'X' ) THEN
            DO 1600 J = 1, JDIM
               DO 1700 I = 1, IDIM
                  I1 = I - 1
                  I2 = I + 1
                  IF( I .EQ. 1    ) I1 = IDIM
                  IF( I .EQ. IDIM ) I2 = 1
                  GX( I,J,K ) = W0 * GY( I,J,K )
     &                        + W1 * GY( I1,J,K ) + W1 * GY( I2,J,K ) 
 1700          CONTINUE
 1600       CONTINUE
         ELSEIF( CSMD(1:1) .EQ. 'Y' ) THEN
            DO 1800 I = 1, IDIM
               DO 1900 J = 1, JDIM
                  J1 = J - 1
                  J2 = J + 1
                  IF( J .EQ. 1    ) J1 = J
                  IF( J .EQ. JDIM ) J2 = J
                  GX( I,J,K ) = W0 * GY( I,J,K )
     &                        + W1 * GY( I,J1,K ) + W1 * GY( I,J2,K ) 
 1900          CONTINUE
 1800       CONTINUE
         ENDIF
 1500 CONTINUE

      RETURN
      END
