* PACKAGE PVDFM  !! physics: vertical diffusion
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/13(numaguti) unit of QFLUX
*         96/10/24(numaguti) default of FMRIB1
*         96/11/19(numaguti) FMRIB
*         96/11/21(numaguti) DELTP
*         96/12/16(numaguti) SEDIM
***********************************************************************
      SUBROUTINE VDFFLX    !! vertical diffusion
     O         ( UFLUX , VFLUX , TFLUX , QFLUX ,
     O           DUFLUX,         DTFLUX, DQFLUX,
     I           GDU   , GDV   , GDT   , GDQ   , 
     I           GDTV  , GDTVM , GDCFRC,
     I           GDP   , GDPM  , GDZ   , GDZM  , 
     I           DELTP , IJS                     )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     UFLUX ( IJSDIM, KMAX+1       ) !! flux of U
      REAL*8     VFLUX ( IJSDIM, KMAX+1       ) !! flux of V
      REAL*8     TFLUX ( IJSDIM, KMAX+1       ) !! flux of T
      REAL*8     QFLUX ( IJSDIM, KMAX+1, NTR  ) !! flux of q
      REAL*8     DUFLUX( IJSDIM, KMAX+1, -1:0 ) !! impl. mat. of U
      REAL*8     DTFLUX( IJSDIM, KMAX+1, -1:0 ) !! impl. mat. of T
      REAL*8     DQFLUX( IJSDIM, KMAX+1, -1:0 ) !! impl. mat. of q
*
*   [INPUT] 
      REAL*8     GDU   ( IJSDIM, KMAX      )   !! westerly U
      REAL*8     GDV   ( IJSDIM, KMAX      )   !! southern wind V
      REAL*8     GDT   ( IJSDIM, KMAX      )   !! temperature T
      REAL*8     GDQ   ( IJSDIM, KMAX, NTR )   !! humidity q
      REAL*8     GDTV  ( IJSDIM, KMAX      )   !! virtual temperature
      REAL*8     GDTVM ( IJSDIM, KMAX+1    )   !! temp. (half lev)
      REAL*8     GDCFRC( IJSDIM, KMAX      )   !! ratio of cloudy area
      REAL*8     GDP   ( IJSDIM, KMAX      )   !! pressure P
      REAL*8     GDPM  ( IJSDIM, KMAX      )   !! pressure (half lev)
      REAL*8     GDZ   ( IJSDIM, KMAX      )   !! altitude
      REAL*8     GDZM  ( IJSDIM, KMAX      )   !! altitude (half lev)
      REAL*8     DELTP                         !! time step
      INTEGER    IJS                           !! IJ index
*
*   [INTERNAL WORK] 
#ifndef OPT_NOCOMMON
      COMMON 
     &           DVDZ  , RIB   ,
     &           DFM   , DFH   , DFE   ,
     &           EXNER , EXNERM, GQS   ,
     &           WORK1 , WORK2 , WORK3 
#endif
      REAL*8     DVDZ  ( IJSDIM, KMAX )   !! d|v|/dz
      REAL*8     RIB   ( IJSDIM, KMAX )   !! bulk Ri number
      REAL*8     DFM   ( IJSDIM, KMAX )   !! trans. coef. :u
      REAL*8     DFH   ( IJSDIM, KMAX )   !! trans. coef. :T
      REAL*8     DFE   ( IJSDIM, KMAX )   !! trans. coef. :q
      REAL*8     EXNER ( IJSDIM, KMAX )   !! Exner function : integer
      REAL*8     EXNERM( IJSDIM, KMAX )   !! Exner func.: half lev
      REAL*8     GQS   ( IJSDIM, KMAX )   !! saturation humidity & work 
      REAL*8     WORK1 ( IJSDIM, KMAX )   !! work
      REAL*8     WORK2 ( IJSDIM, KMAX )
      REAL*8     WORK3 ( IJSDIM, KMAX )
*
      INTEGER    IJ, K, LT, KK
      INTEGER    IFPAR, JFPAR
      REAL*8     AKAPPA, FRI, THVK, THVKM, FC
      REAL*8     RIBD, RIBS, RIBX
      REAL*8     RHO, RDZ
*
*   [INTERNAL PARM] 
      REAL*8     P00, THS
      REAL*8     URMIN , RIBMIN, RIBMAX
      REAL*8     FMRIB0, FMRIB1, FMRIB , RIBSMN
      LOGICAL    OFIRST
      NAMELIST  /NMVDFM/ THS   , URMIN , RIBMIN, RIBMAX, 
     &                   FMRIB0, FMRIB1, FMRIB , RIBSMN
*
*   [INTERNAL FUNC]
#include        "zqsat.F"                /* saturate vapor pressure */
      DATA       P00    / 100000. /
      DATA       THS    / 300.    /           !! basic pot. temp
      DATA       URMIN  / 0.1     /           !! min. of del-wind sq.
      DATA       RIBMIN / -100.   /           !! min. of bulk Ri number
      DATA       RIBMAX /  100.   /           !! max. of bulk Ri number
      DATA       FMRIB0 / 0.0     /           !! moist Ri factor
      DATA       FMRIB1 / 0.5     /           !! moist Ri factor
      DATA       RIBSMN /  0.     /           !! min. of wet bulk Ri number
      DATA       OFIRST / .TRUE.  /
*
*          < 1. constant >
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PVDFM: VERTICAL DIFFUSION 93/12/22'
         OFIRST = .FALSE.
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMVDFM, END=1190 )
 1190    WRITE  ( JFPAR, NMVDFM )
      ENDIF
*
      AKAPPA = RAIR / CP
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJSDIM
            EXNER ( IJ,K ) = ( GDP ( IJ,K )/P00 ) ** AKAPPA
            EXNERM( IJ,K ) = ( GDPM( IJ,K )/P00 ) ** AKAPPA
            GQS   ( IJ,K ) = FQSAT( GDT( IJ,K ), GDP( IJ,K ) )
 1100 CONTINUE
*
      CALL RESET( DVDZ, IJSDIM )
      CALL RESET( RIB , IJSDIM )
*
      DO 2100 K = 2, KMAX
         DO 2100 IJ = 1, IJSDIM
            DVDZ( IJ,K ) = SQRT( MAX( URMIN,
     &                               ( GDU( IJ,K )-GDU( IJ,K-1 ) )**2
     &                              +( GDV( IJ,K )-GDV( IJ,K-1 ) )**2
     &                         )    )
     &                       / ( GDZ( IJ,K ) - GDZ( IJ,K-1 ) )
 2100 CONTINUE
*
      DO 2200 K = 2, KMAX
         DO 2200 IJ = 1, IJSDIM
            FRI   = GRAV / THS / ( GDZ( IJ,K ) - GDZ( IJ,K-1 ) )
     &                         / DVDZ ( IJ,K )**2
*
            THVK  = GDTV( IJ,K   )/EXNER( IJ,K   )
            THVKM = GDTV( IJ,K-1 )/EXNER( IJ,K-1 )
            RIBD  = FRI * ( THVK - THVKM )
*
            THVK  = THVK  + EL/CP*GQS( IJ,K   )/EXNER( IJ,K   )
            THVKM = THVKM + EL/CP*GQS( IJ,K-1 )/EXNER( IJ,K-1 )
            RIBS  = FRI * ( THVK - THVKM )
            RIBS  = MAX( RIBS, RIBSMN )
*
            FC    = FMRIB1*GDCFRC( IJ,K-1 ) + FMRIB0*GDCFRC( IJ,K )
            FC    = MIN( MAX( FC, 0.D0 ), 1.D0 )
*
            RIBX  = RIBD + FC * MIN( RIBS - RIBD, 0.D0 )
            RIB  ( IJ,K ) = MIN( MAX ( RIBX, RIBMIN ), RIBMAX )
            WORK1( IJ,K ) = RIBD
            WORK2( IJ,K ) = RIBS
 2200 CONTINUE
*
      CALL HISTIN
     I   ( WORK1, 'RIBD' , 'bulk Ri No. DRY' ,'N.D.', 'ALEV')
      CALL HISTIN
     I   ( WORK2, 'RIBS' , 'bulk Ri No. WET' ,'N.D.', 'ALEV')
      CALL HISTIN
     I   ( RIB,   'RIB'  , 'bulk Ri No.' ,'N.D.', 'ALEV')
*
      CALL VDFCOF
     O         ( DFM   , DFH   , DFE   ,
     I           RIB   , DVDZ  , GDZM  ,
     W           WORK1 , WORK2 , WORK3 , GQS   )
*
      CALL SHLCOF !! diffusive shallow cumulus
     M         ( DFM   , DFH   , DFE   ,
     I           GDT   , GDQ   , GDP   , GDZ   ,
     W           WORK1 , WORK2 , WORK3 , GQS    )
*
      CALL HISTIN
     I   ( DFM, 'DFM'  , 'diffusion coef. momentum' ,'m**2/s', 'ALEV')
      CALL HISTIN
     I   ( DFH, 'DFH'  , 'diffusion coef. heat    ' ,'m**2/s', 'ALEV')
      CALL HISTIN
     I   ( DFE, 'DFE'  , 'diffusion coef. moisture' ,'m**2/s', 'ALEV')
*
*          < 4. transfer coef. >
*
      DO 4100 K = 2, KMAX
         DO 4100 IJ = 1, IJSDIM
            RHO         = GDPM( IJ,K ) / RAIR / GDTVM( IJ,K )
            RDZ         = RHO /( GDZ( IJ,K ) - GDZ( IJ,K-1 ) )
            DFM( IJ,K ) = RDZ * DFM( IJ,K )
            DFH( IJ,K ) = RDZ * DFH( IJ,K )
            DFE( IJ,K ) = RDZ * DFE( IJ,K )
 4100 CONTINUE
*
*          < 5. fluxes >
*
      DO 5100 K = 2, KMAX
         DO 5100 IJ = 1, IJSDIM
            UFLUX( IJ,K ) = DFM( IJ,K )
     &                       * ( GDU ( IJ,K-1 ) - GDU ( IJ,K ) )
*
            VFLUX( IJ,K ) = DFM( IJ,K )
     &                       * ( GDV ( IJ,K-1 ) - GDV ( IJ,K ) )
*
            TFLUX( IJ,K ) = CP * DFH( IJ,K )  * EXNERM( IJ,K   )
     &                        * (  GDT( IJ,K-1 ) / EXNER ( IJ,K-1 )
     &                           - GDT( IJ,K   ) / EXNER ( IJ,K   ) )
 5100 CONTINUE
*
      DO 5200 LT = 1, NTR
         DO 5200 K = 2, KMAX
            DO 5200 IJ = 1, IJSDIM
               QFLUX( IJ,K,LT ) = DFE( IJ,K )
     &                       * ( GDQ( IJ,K-1,LT ) - GDQ( IJ,K,LT ) )
 5200 CONTINUE 
*
*          < 6. implicit matrix >
*
      DO 6100 K = 2, KMAX
         DO 6100 IJ = 1, IJSDIM
            DUFLUX( IJ,K, 0 ) = -      DFM( IJ,K )
            DUFLUX( IJ,K,-1 ) =        DFM( IJ,K )
*
            DTFLUX( IJ,K, 0 ) = - CP * DFH( IJ,K )*EXNERM( IJ,K   )
     &                                            /EXNER ( IJ,K   )
            DTFLUX( IJ,K,-1 ) =   CP * DFH( IJ,K )*EXNERM( IJ,K   )
     &                                            /EXNER ( IJ,K-1 )
*
            DQFLUX( IJ,K, 0 ) = -      DFE( IJ,K )
            DQFLUX( IJ,K,-1 ) =        DFE( IJ,K )
 6100 CONTINUE
*
      DO 6300 K = 1, KMAX+1, KMAX
         DO 6300 KK = -1, 0
            CALL RESET( DUFLUX(1,K,KK), IJSDIM )
            CALL RESET( DTFLUX(1,K,KK), IJSDIM )
            CALL RESET( DQFLUX(1,K,KK), IJSDIM )
 6300 CONTINUE 
*
*   < sedmentation >
*
      CALL SEDIMT         !! sedimentation of tracer
     M         ( QFLUX ,
     I           GDQ   , GDTVM , GDPM  , 
     I           DELTP , IJS            )
*
      RETURN
      END
