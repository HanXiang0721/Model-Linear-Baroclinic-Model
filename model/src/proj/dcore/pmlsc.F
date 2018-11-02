* PACKAGE PMLSC !! physics: large scale condensation with cloud water
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/06/24(numaguti) SQRT -> **(2/3)
*         96/05/09(numaguti) speed up
*         96/05/13(numaguti) Tracer scavenging
*         96/09/18(numaguti) GSIGD
*         96/11/11(numaguti) treatment of ice
*         96/11/20(numaguti) re-diag GDCFRC
*         97/01/29(numaguti) OPT_WTRACE
*         97/07/29(numaguti) output FPRCL
**********************************************************************
      SUBROUTINE LSCOND    !! large scale condensation
     O         ( GTT   , GTQ   ,
     O           GPRCL , GSNWL , GDCLW , GDCFRC, 
     I           GDT   , GDQ   , GDTM  , GDSIGD,
     I           GDP   , GDPM  , GDZ   , GDZM  , 
     I           DELT                             )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT]
      REAL*8     GTT   ( IJSDIM, KMAX      )   !! heating rate
      REAL*8     GTQ   ( IJSDIM, KMAX, NTR )   !! change in q & l
      REAL*8     GPRCL ( IJSDIM, NTR  )        !! rain fall
      REAL*8     GSNWL ( IJSDIM       )        !! snow fall
      REAL*8     GDCLW ( IJSDIM, KMAX )        !! cloud water
      REAL*8     GDCFRC( IJSDIM, KMAX )        !! cloudiness
*
*   [INPUT] 
      REAL*8     GDT   ( IJSDIM, KMAX      )   !! temperature T
      REAL*8     GDQ   ( IJSDIM, KMAX, NTR )   !! humidity q
      REAL*8     GDTM  ( IJSDIM, KMAX+1    )   !! temperature T
      REAL*8     GDSIGD( IJSDIM, KMAX+1    )   !! sigma velocity
      REAL*8     GDP   ( IJSDIM, KMAX      )   !! pressure P
      REAL*8     GDPM  ( IJSDIM, KMAX+1    )   !! pressure (half lev)
      REAL*8     GDZ   ( IJSDIM, KMAX      )   !! altitude
      REAL*8     GDZM  ( IJSDIM, KMAX+1    )   !! altitude
      REAL*8     DELT                          !! delta(t) (atmosphere)
*
*   [INTERNAL WORK] 
#ifndef OPT_NOCOMMON
      COMMON 
     &           PRCP, EVP, GPREC
#endif
      REAL*8     PRCP  ( IJSDIM, KMAX )   !! formation of precipitation
      REAL*8     EVP   ( IJSDIM, KMAX )   !! evapolation
      REAL*8     GPREC ( IJSDIM, KMAX+1 ) !! precipitation flux
      INTEGER    IJ, K, LT, LTX
      INTEGER    IFPAR, JFPAR
      REAL*8     ELF, ELFI, ELFM
      REAL*8     FLIQ, QLIQ
      REAL*8     PRCEF, RZ, AML, CLWM
      REAL*8     QSAT, GAM, GDQW
      REAL*8     RDZ, EVPF, EVPP, EVPS
      REAL*8     GDTW, EVPC, GDTL, GDQL, GDQNEG
      REAL*8     RHL, AA, BB, GDCLWX, GDTX, GDQX, DQR
      REAL*8     QICE, RHO, VICE, PRIEF
      REAL*8     GPRX, GPRCLX, SCAV
*      
*   [INTERNAL PARM] 
      REAL*8     PRCCRL                   !! crit. cloud water
      REAL*8     PRCTAU                   !! relaxation t of precip.
      REAL*8     PRCCOL                   !! collection coeff.
      REAL*8     EVAPR                    !! evaporation factor
      REAL*8     VTERM                    !! term. vel. of precip.
      REAL*8     DQRAT                    !! variation rate of vapor
      REAL*8     AML0                     !! mixing length
      REAL*8     TCRICE                   !! crit. temp. of ice
      REAL*8     TEFICE                   !! temp where ice ratio=1-1/e
      REAL*8     VICE0                    !! deposition vel. of ice
      REAL*8     VICEP                    !! deposition vel. of ice
      INTEGER    KLMAX                    !! probable area
      LOGICAL    OSQRTC                   !! cloudiness = sqrt
      REAL*8     TWSNOW                   !! wet-bulb temp. rain/snow
      INTEGER    NTRX
      PARAMETER (NTRX=NTR-ITL+1)
      REAL*8     FSCAV ( NTRX )
      REAL*8     FENTR ( NTRX )
      REAL*8     FSCUM ( NTRX )
      REAL*8     FSWTR ( NTRX )
      LOGICAL    OFIRST
      NAMELIST  /NMMLSC/ PRCCRL, PRCTAU, PRCCOL,
     &                   EVAPR , VTERM , 
     &                   DQRAT , AML0  , 
     &                   TCRICE, TEFICE, VICE0 , VICEP , 
     &                   KLMAX , OSQRTC
      NAMELIST  /NMSNWF/ TWSNOW
      NAMELIST  /NMSCAV/ FSCAV , FENTR , FSCUM ,  FSWTR
*
*   [INTERNAL FUNC] 
#include        "zqsat.F"                /* saturate vapor pressure */
      DATA       PRCCRL         / 3.E-4          /
      DATA       PRCTAU         / 10000.         /
      DATA       PRCCOL         / 1.D0           /
      DATA       EVAPR , VTERM  / 1.0, 10.       /
      DATA       DQRAT          / 0.3            /
      DATA       AML0           / 300.           /
      DATA       TCRICE, TEFICE / 273.15, 255.15 /
      DATA       VICE0 , VICEP  / 6.0, 0.17      /
      DATA       KLMAX          / KMAX           /
      DATA       OSQRTC         / .TRUE.   /
      DATA       TWSNOW         / 273.15   /
      DATA       FSCAV          / NTRX*0.0 /
      DATA       FENTR          / NTRX*0.0 /
      DATA       FSCUM          / NTRX*0.0 /
#ifdef OPT_WTRACE
      DATA       FSWTR          / NTRX*1.0 /
#else
      DATA       FSWTR          / NTRX*0.0 /
#endif
      DATA       OFIRST         / .TRUE.   /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PMLSC: LARGE SCALE COND. 96/11/27'
         OFIRST = .FALSE.
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMMLSC, END=190 )
  190    WRITE  ( JFPAR, NMMLSC )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSNWF, END=290 )
  290    WRITE  ( JFPAR, NMSNWF )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSCAV, END=390 )
  390    WRITE  ( JFPAR, NMSCAV )
      ENDIF
*
      ELF  = EL / CP
      ELFI = (EL+EMELT) / CP
      ELFM = EMELT / CP
*
      CALL RESET( GTT,    IJSDIM*KMAX     )
      CALL RESET( GTQ,    IJSDIM*KMAX*NTR ) 
      CALL RESET( GDCLW,  IJSDIM*KMAX     )
      CALL RESET( GDCFRC, IJSDIM*KMAX     ) 
      CALL RESET( PRCP,   IJSDIM*KMAX     )
      CALL RESET( EVP,    IJSDIM*KMAX     ) 
      CALL RESET( GSNWL,  IJSDIM          )
      CALL RESET( GPRCL,  IJSDIM*NTR      )
      CALL RESET( GPREC,  IJSDIM*(KMAX+1) )
*
      DO 2000 K = KLMAX, 1, -1
         DO 2000 IJ = 1, IJSDIM
*
            RZ    = GDZ( IJ,K ) - GDZM( IJ,1 )
            AML   = FKARM * RZ  / ( 1.D0 + FKARM*RZ/AML0 )
            DQR   = DQRAT*AML/AML0
            RDZ   = ( GDPM( IJ,K )-GDPM( IJ,K+1 ) )/GRAV
*
*           << evap of rain >>
*
            QSAT  = FQSAT( GDT( IJ,K ), GDP( IJ,K ) )
            GAM   = ELF*FDQSAT( GDT( IJ,K ), QSAT  )
            GDQW  = GDQ( IJ,K,1 ) + ( QSAT - GDQ( IJ,K,1 ) )/(1.+GAM)
*
            EVPF  = EVAPR*MAX( GDQW-GDQ(IJ,K,1), 0.D0 )/VTERM
            EVPF  = MIN( EVPF, 1.D0/RDZ )
            EVPP  = GPRCL( IJ,1 )*EVPF
            EVPS  = GSNWL( IJ   )*EVPF
            GPRCL( IJ,1 ) = GPRCL( IJ,1 ) - EVPP*RDZ 
            GSNWL( IJ   ) = GSNWL( IJ   ) - EVPS*RDZ
            EVP  ( IJ,K ) = EVPP+EVPS
*
*           << evap of cloud >>
*
            EVPC   = GDQ( IJ,K,2 )/DELT
            GDTL   = GDT( IJ,K   ) - ELF *(EVPP+EVPC)*DELT
     &                             - ELFI* EVPS      *DELT
            GDQL   = GDQ( IJ,K,1 ) + (EVPP+EVPC+EVPS)*DELT
            GDQNEG = MIN( GDQL, 0.D0 )
            GDQL   = MAX( GDQL, 0.D0 )
*
*           << diag. cloud water >>
*
            GDTX  = GDTL
            QSAT  = FQSAT( GDTX, GDP( IJ,K ) )
            RHL   = GDQL/QSAT
            AA    = ( (1.D0+DQR)*RHL - 1.D0 )*0.5D0
            BB    =   (1.D0-DQR)*RHL
*
            IF      ( AA .LE. 0.D0 ) THEN
               GDCLWX = 0.D0
            ELSE IF ( BB .GT. 1.D0 ) THEN
               GDCLWX = ( RHL-1.D0 )/( 1.D0+GAM  )*QSAT
            ELSE 
               GDCLWX = AA*AA/( DQR*RHL + AA*GAM )*QSAT
            ENDIF
*
#ifdef OPT_ITERATE_PMLSC
            GDTX  = GDTL + ELF * GDCLWX
            QSAT  = FQSAT ( GDTX, GDP( IJ,K ) )
            RHL   = GDQL/QSAT
            AA    = ( (1.D0+DQR)*RHL - 1.D0 )*0.5D0
            BB    =   (1.D0-DQR)*RHL
*
            IF      ( AA .LE. 0.D0 ) THEN
               GDCLWX = 0.D0
            ELSE IF ( BB .GT. 1.D0 ) THEN
               GDCLWX = ( RHL-1.D0 )/( 1.D0+GAM  )*QSAT
            ELSE 
               GDCLWX = AA*AA/( DQR*RHL + AA*GAM )*QSAT
            ENDIF
#endif
*
            GDTX  = GDTL + ELF * GDCLWX
            QSAT  = FQSAT( GDTX,GDP(IJ,K) )
            RHL   = GDQL/QSAT
            AA    = ( (1.D0+DQR)*RHL - 1.D0 )*0.5D0
            BB    =   (1.D0-DQR)*RHL
*
            IF      ( AA .LE. 0.D0  ) THEN
               GDCFRC( IJ,K ) = 0.D0
               GDCLW ( IJ,K ) = 0.D0
            ELSE IF ( BB .GE. 1.D0 ) THEN
               GDCFRC( IJ,K ) = 1.D0
               GDCLW ( IJ,K ) = GDQL - QSAT
            ELSE 
               GDCFRC( IJ,K ) = AA/( DQR*RHL )
               GDCLW ( IJ,K ) = AA*GDCFRC( IJ,K )*QSAT
            ENDIF
            IF ( OSQRTC ) THEN
               GDCFRC( IJ,K ) = GDCFRC( IJ,K )**0.667
            ENDIF
*
            GDTX = GDTL + ELF * GDCLW( IJ,K )
            GDQX = GDQL - GDCLW( IJ,K ) + GDQNEG
*
*        << liquid/ice partition >>
*
            FLIQ  = MAX( (TCRICE-GDTX)/(TCRICE-TEFICE), 0.D0 )
            FLIQ  = EXP( -FLIQ*FLIQ )
            QLIQ  = FLIQ*GDCLW( IJ,K )
            QICE  = GDCLW( IJ,K ) - QLIQ
*
*        << liquid precipitation >>
*
            IF ( GDCFRC( IJ,K ) .GT. 0. ) THEN
               CLWM  = ( QLIQ/GDCFRC( IJ,K )  )/PRCCRL
               PRCEF = ( 1.D0-EXP(-CLWM*CLWM) )/PRCTAU 
     &               + GPREC( IJ,K+1 )*PRCCOL
               PRCEF = TANH( PRCEF*DELT )/DELT 
               PRCP( IJ,K ) = QLIQ*PRCEF
            ELSE
               PRCP( IJ,K ) = 0.D0
            ENDIF
*
*        << ice fall >>
*
            IF ( QICE .GT. 0. ) THEN
               RHO    = GDP( IJ,K )/(RAIR*GDTX)
               VICE   = VICE0*( RHO*QICE )**VICEP
               PRIEF  = ( RHO*VICE )/RDZ
               PRIEF  = TANH( PRIEF*DELT )/DELT
               PRCP( IJ,K ) = PRCP( IJ,K ) + QICE*PRIEF
            ENDIF
*            
*        << total precipitation >>
*
            GDCLW( IJ,K ) = GDCLW( IJ,K ) - PRCP( IJ,K )*DELT 
            GPRCL( IJ,1 ) = GPRCL( IJ,1 ) + PRCP( IJ,K )*RDZ
            GPREC( IJ,K ) = GPRCL( IJ,1 ) + GSNWL( IJ )
*
*        << melt/freeze >>
*
            QSAT  = FQSAT( GDTX, GDP( IJ,K ) )
            GAM   = ELF*FDQSAT( GDTX, QSAT  )
            GDTW  = GDTX - ELF*( QSAT - GDQX )/( 1.+GAM )
*
            IF ( GDTW .LT. TWSNOW ) THEN
               GDTX = GDTX + ELFM* GPRCL( IJ,1 )*DELT/RDZ
               GPRCL( IJ,1 ) = 0.D0
               GSNWL( IJ   ) = GPREC( IJ,K )
            ELSE
               GDTX = GDTX - ELFM* GSNWL( IJ )*DELT/RDZ
               GPRCL( IJ,1 ) = GPREC( IJ,K )
               GSNWL( IJ   ) = 0.D0
            ENDIF
*
*        << re-diagnose cldfrc >>
*
            IF ( GDCLW( IJ,K ) .GT. 0. ) THEN
               RHL   = ( GDQX + GDCLW( IJ,K ) )/QSAT
               AA    = ( (1.D0+DQR)*RHL - 1.D0 )*0.5D0
               BB    =   (1.D0-DQR)*RHL
               IF      ( AA .LE. 0.D0  ) THEN
                  GDCFRC( IJ,K ) = 0.D0
                  GDQX           = GDQX + GDCLW( IJ,K )
                  GDCLW ( IJ,K ) = 0.D0
               ELSE IF ( BB .GE. 1.D0 ) THEN
                  GDCFRC( IJ,K ) = 1.D0
               ELSE 
                  GDCFRC( IJ,K ) = AA/( DQR*RHL )
               ENDIF
               IF ( OSQRTC ) THEN
                  GDCFRC( IJ,K ) = GDCFRC( IJ,K )**0.667
               ENDIF
            ELSE
               GDCLW ( IJ,K ) = 0.
               GDCFRC( IJ,K ) = 0.
            ENDIF
*
*        << tendency term >>
*
            GTQ( IJ,K,1 ) = ( GDQX          - GDQ( IJ,K,1 ) )/DELT 
            GTQ( IJ,K,2 ) = ( GDCLW( IJ,K ) - GDQ( IJ,K,2 ) )/DELT 
            GTT( IJ,K   ) = ( GDTX          - GDT( IJ,K   ) )/DELT 
 2000 CONTINUE
*
*     << scavenging of tracer >>
*
      DO 4000 LT = ITL+1, NTR
         LTX = LT - ITL
         DO 4100 K = KLMAX, 1, -1
            DO 4100 IJ = 1, IJSDIM
               RDZ  = ( GDPM( IJ,K ) - GDPM( IJ,K+1 ) )/GRAV
               GPRX =  GPREC( IJ,K+1 ) - EVP( IJ,K )*RDZ
               IF ( GPREC( IJ,K+1 ) .GT. 0.D0 ) THEN
                  GPRCLX = GPRCL( IJ,LT )*GPRX/GPREC( IJ,K+1 )
               ELSE
                  GPRCLX = 0.D0
               ENDIF
               SCAV = FENTR( LTX )*PRCP( IJ,K )*RDZ 
     &              + FSCAV( LTX )*GPRX        *RDZ 
               GDQL = GDQ( IJ,K,1 ) + GDQ( IJ,K,2 )
               IF ( GDQL .GT. 0.D0 ) THEN
                  SCAV = SCAV + FSWTR( LTX )*PRCP( IJ,K )*RDZ/GDQL
               ENDIF
               SCAV             = MIN( SCAV, RDZ/DELT )
               GPRCLX           = GPRCLX + GDQ( IJ,K,LT ) * SCAV
               GTQ  ( IJ,K,LT ) = ( GPRCL( IJ,LT ) - GPRCLX )/RDZ
               GPRCL( IJ,LT   ) = GPRCLX 
 4100    CONTINUE 
 4000 CONTINUE 
*
      CALL HISTIN
     I       ( PRCP, 'PRLSC', 'LSC precipitation ' ,'1/S', 'ALEV'  )
      CALL HISTIN
     I       ( EVP , 'EVLSC', 'LSC evaporation   ' ,'1/S', 'ALEV'  )
      CALL HISTIN
     I       ( GPREC,'FPRCL', 'LSC precip. flux' ,'Kg/m**2/s','AMLEV')
*
      RETURN
      END
