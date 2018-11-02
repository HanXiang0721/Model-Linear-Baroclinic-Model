* PACKAGE DCIRC !! dynamics: terms of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         04/03/30(hiro)     add MTRUNC
**********************************************************************
      SUBROUTINE DYNTRM    !! dynamics terms
     O         ( WTVOR , WTDIV , WTT   , WTPI  ,
     O           GSIGD , GPID  , GAVOR , GADIV , GATV  , GAW  ,
     O           TFLX  ,
     I           GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     I           GRFV  , GRFD  , GRFT  , GRFP  ,
     I           GBT   ,
     I           TIME  , OANM  , RSTT  , FCTT  ,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA,
     W           GTUX  , GTVX  , GTXX  , WTKE           )
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
*
      REAL*8     GSIGD ( IJDIM, KMAX+1     )  !! tendency of sigma
      REAL*8     GPID  ( IJDIM             )  !! tendency of pai
      REAL*8     GAVOR ( IJDIM, KMAX       )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX       )  !! divergence D 
      REAL*8     GATV  ( IJDIM, KMAX       )  !! virtulal tmp.
      REAL*8     GAW   ( IJDIM, KMAX       )  !! p-velocity
      REAL*8     TFLX  ( IJDIM, KMAX       )  !! Newtonian cooling
*
*   [INPUT] 
      REAL*8     GAU   ( IJDIM, KMAX       )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX       )  !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX       )  !! temperature T
      REAL*8     GAPS  ( IJDIM             )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX, NTR  )  !! virtual temperature
*
      REAL*8     GRFV ( IJDIM, KMAX )        !! R-forcing for vor.
      REAL*8     GRFD ( IJDIM, KMAX )        !! R-forcing for div.
      REAL*8     GRFT ( IJDIM, KMAX )        !! R-forcing for temp.
      REAL*8     GRFP ( IJDIM       )        !! R-forcing for LnPs

      REAL*8     GBT   ( IJDIM, KMAX       )  !! temperature T
*
      REAL*8     SALPHA( KMAX  )             !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX  )             !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX  )             !! fact. of vert. intrp. T
      REAL*8     ATF   ( KMAX  )             !! fact. of vert. intrp. T
      REAL*8     BTF   ( KMAX  )             !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX  )             !! basic temp. (full lev)
      REAL*8     TMTX  ( KMAX, KMAX )        !! for semi-implicit
*
      REAL*8     CORIOL( IJDIM )             !! Corioris factor f
      REAL*8     FLAPLA( NMDIM )             !! Laplacian factor
*
      REAL*8     RSTT  ( KMAX   )         !! restoring to T_te
      REAL*8     FCTT
*
      REAL*8     TIME                        !! time
      LOGICAL    OANM                        !! anomaly output?
*
*   [WORK] 
      REAL*8     GTUX  ( IJDIM, KMAX   )  !! tend. zonal  adv.
      REAL*8     GTVX  ( IJDIM, KMAX   )  !! tend. merid. adv.
      REAL*8     GTXX  ( IJDIM, KMAX   )  !! tend.
      REAL*8     WTKE  ( NMDIM, KMAX   )  !! KE term
      REAL*8     WRK3  ( NMDIM, KMAX   )  !! work
      REAL*8     WRK1  ( NMDIM         )  !! work
*
*   [INTERNAL WORK]
      REAL*8     GAPADV( IJDIM, KMAX )   !! advection of pai
      REAL*8     GSUMP ( IJDIM, KMAX )   !! vert. integ. of tend. of pi
      REAL*8     GAPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GAPIY ( IJDIM       )   !! merid. differ. of pi
      REAL*8     GWX   ( IJDIM, KMAX )   !! work
      REAL*8     GWY   ( IJDIM, KMAX )   !! work

      REAL*8     FACA, PADV
      INTEGER    IJ, K, L, NM
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL PARAM]
      INTEGER    KTRUN
      LOGICAL    OTRUN
      NAMELIST / NMTRUN / OTRUN, KTRUN
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE.  /
      DATA       OTRUN  / .FALSE. / 
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DTERM: DYNAMICS EQ. TERMS 96/05/28'
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMTRUN, END=180 )
 180     WRITE  ( JFPAR, NMTRUN )
         OFIRST = .FALSE.
      ENDIF
*
*     zonal wavenumber truncation
*
      IF( OTRUN ) THEN
         CALL MTRUNC( GAU , KMAX, KTRUN, WRK3 )
         CALL MTRUNC( GAV , KMAX, KTRUN, WRK3 )
         CALL MTRUNC( GAT , KMAX, KTRUN, WRK3 )
         CALL MTRUNC( GAPS, 1   , KTRUN, WRK1 )
         CALL MTRUNC( GAQ , KMAX, KTRUN, WRK3 )
      ENDIF
*
*    << circulations >>
*
      CALL UV2VDG
     O         ( GAVOR , GADIV ,
     I           GAU   , GAV   , .TRUE., .TRUE., OANM  ,
     W           GTUX  , GTVX  , WTVOR , WTDIV           )
*
      DO 1100 IJ = 1, IJDIM
         GTXX( IJ,1 ) = LOG( GAPS( IJ ) )
 1100 CONTINUE 
      CALL HGRAD                       !! surface pressure gradient
     O         ( GAPIX , GAPIY ,
     I           GTXX  , 1     ,
     W           WTKE           )
*
      CALL PSDOT     !!  surface pres. change & sigma velocity
     O         ( GSIGD , GSUMP , GAPADV, GPID  ,
     I           GAU   , GAV   , GADIV , GAPIX , GAPIY  )
*
*    << pressure eq. >>
*
      CALL COPY ( GTXX, GPID, IJDIM )
      DO 2100 K = 1, KMAX
         DO 2100 IJ = 1, IJDIM
            GTXX( IJ,1 ) = GTXX( IJ,1 ) + DSIG( K )*GADIV( IJ,K )
 2100 CONTINUE 
      DO 2200 IJ = 1, IJDIM
         GTXX( IJ,1 ) = GTXX( IJ,1 ) + GRFP( IJ ) !! add forcing R
 2200 CONTINUE 
*
      CALL G2W
     O         ( WTPI  ,
     I           GTXX  ,
     I           '    ', 'POSO',  1  )
*
*    << temperature eq. >>
*
      CALL VIRTMD
     O         ( GATV  ,
     I           GAT   , GAQ     )
*
      CALL GRTADV                      !!  temperature advection
     O         ( GTUX  , GTVX  , GTXX  ,
     I           GAT   , GATV  ,
     I           GAU   , GAV   , GADIV , 
     I           GSIGD , GSUMP , GAPADV, 
     C           SALPHA, SBETA , SKAPPA,
     C           ATF   , BTF   , TBAR    )
*
      DO 3100 L = 1, KMAX
         DO 3100 K = 1, KMAX
            DO 3100 IJ = 1, IJDIM
               GTXX( IJ,K ) = GTXX( IJ,K ) + TMTX( K,L )*GADIV( IJ,L )
 3100 CONTINUE 
      DO 3200 K = 1, KMAX
         DO 3200 IJ = 1, IJDIM
*
*     modified on 09/30/03
            TFLX( IJ,K ) = 
     &           RSTT( K ) * ( GRFT( IJ,K ) - GBT( IJ,K ) )
            GTXX( IJ,K ) =   GTXX( IJ,K ) 
     &                     + FCTT * GRFT( IJ,K ) !! add forcing R
     &                     + TFLX( IJ,K ) !! add restoring forcing
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
     I           GAPIX , GAPIY , GSIGD , GATV  , GAT   ,
     C           CORIOL, 
     C           SALPHA, SBETA , SKAPPA, TBAR           )
*
      CALL       UV2VDW    !! (U,V) -> (vor.,D)spectral
     O         ( WTVOR , WTDIV ,
     I           GTUX  , GTVX  , .TRUE., .TRUE.,
     W           GWX   , GWY                            )
*
      CALL G2W                  !! add forcing R
     M         ( WTVOR ,
     I           GRFV  , '    ', 'ADDO', KMAX   )
      CALL G2W                  !! add forcing R
     M         ( WTDIV ,
     I           GRFD  , '    ', 'ADDO', KMAX   )
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
*    << diagnostic for omega >>
*
      FACA = SIG( KMAX ) - SIGM( KMAX+1 )
      DO 5000 IJ = 1, IJDIM
         PADV = ( GADIV( IJ,KMAX ) + GAPADV( IJ,KMAX ) ) * FACA
         GAW( IJ,KMAX ) = 
     &        ( SIG( KMAX )*GAPADV( IJ,KMAX ) - PADV ) * GAPS( IJ ) 
 5000 CONTINUE
      DO 5100 K = 1, KMAX-1
         DO 5100 IJ = 1, IJDIM
            FACA = SIG( K ) - SIGM( K+1 )
            PADV = ( GADIV( IJ,K ) + GAPADV( IJ,K ) ) * FACA
            GAW( IJ,K ) = 
     &           ( SIG(K)*GAPADV( IJ,K )-(GSUMP( IJ,K+1 )+PADV) )
     &           * GAPS( IJ )
 5100 CONTINUE
*
*
      RETURN
      END
***********************************************************************
      SUBROUTINE MTRUNC    !! truncate at m=MTR
     M         ( GX      ,
     I           KDIMD   ,  MTRN   , 
     W           WX         )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zcord.F"                /* coordinate             */
*
*   [INPUT] 
      INTEGER    KDIMD
      INTEGER    MTRN
*
*   [MODIFY] 
      REAL*8     GX  ( IJDIM, KDIMD ) !! grid value
*
*   [WORK] 
      REAL*8     WX  ( NMDIM, KDIMD ) !! wave
*
*   [INTERNAL WORK] 
      INTEGER    L, M, LEND, K
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL SAVE] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      SAVE       NMO
*
*   [INTERNAL PARM] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* )
     $        ' @@@ MTRUNC: TRUNCATION AT ZONAL WAVE #:',MTRN
         CALL DSETNM
     O         ( NMO   )
      ENDIF
*
      CALL G2W
     O     ( WX    ,
     I       GX    ,
     I       '    ', 'POSO', KDIMD )
*
      DO 1000 K = 1, KDIMD
         DO 1100 M = MTRN+1 , MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
            DO 1110 L = 0 , LEND
               WX( NMO(1,M,L),K )  = 0.D0
               WX( NMO(2,M,L),K )  = 0.D0
 1110       CONTINUE
 1100    CONTINUE
 1000 CONTINUE 
*
      CALL W2G 
     O     ( GX    ,
     I       WX    ,
     I       '    ', 'POSO', KDIMD )
*
      RETURN
      END
