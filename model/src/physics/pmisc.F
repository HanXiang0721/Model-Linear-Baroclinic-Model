* PACKAGE PMISC   !! physics: etc.
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
**********************************************************************
      SUBROUTINE PSETUP    !! setup for physics
     O         ( GDTV  , GDTM  , GDTVM ,
     O           GDP   , GDPM  , GDZ   , GDZM  ,
     I           GDT   , GDQ   , GDPS  , GDZS   )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc.           */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     GDTV   ( IJSDIM, KMAX   ) !! virtual temperature Tv
      REAL*8     GDTM   ( IJSDIM, KMAX+1 ) !! temp. (half lev)
      REAL*8     GDTVM  ( IJSDIM, KMAX+1 ) !! virt. temp.(half lev)
      REAL*8     GDP    ( IJSDIM, KMAX   ) !! pressure P
      REAL*8     GDPM   ( IJSDIM, KMAX+1 ) !! pressure (half lev)
      REAL*8     GDZ    ( IJSDIM, KMAX   ) !! altitude
      REAL*8     GDZM   ( IJSDIM, KMAX+1 ) !! altitude (half lev)
*
*   [INPUT] 
      REAL*8     GDT    ( IJSDIM, KMAX      ) !! temperature T
      REAL*8     GDQ    ( IJSDIM, KMAX, NTR ) !! humidity etc.
      REAL*8     GDPS   ( IJSDIM            ) !! surface pressure Ps
      REAL*8     GDZS   ( IJSDIM )            !! surface topography
*
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PMISC: PHYSICS SETUP 94/11/25'
         OFIRST = .FALSE.
      ENDIF
*
      CALL VIRTM
     O         ( GDTV  ,
     I           GDT   , GDQ    )
*
      CALL TINTP
     O         ( GDTM  ,
     I           GDT    )
*
      CALL TINTP
     O         ( GDTVM ,
     I           GDTV    )
*
      CALL PLEVS
     O         ( GDP   , GDPM ,
     I           GDPS           )
*
      CALL ZLEVS
     O         ( GDZ   , GDZM  ,
     I           GDTV  , GDTVM , GDZS   )
*
      RETURN
      END
***********************************************************************
      SUBROUTINE VIRTM    !! virtual temperature (physics)
     O         ( GDTV  ,
     I           GDT   , GDQ     )
*
*               NTR=2 : q(LT=1), l(LT=2)
*               NTR=1 : only q
*
*   [PARAM] 
#include        "zcdim.F"
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"
*
*   [OUTPUT]
      REAL*8     GDTV  ( IJSDIM, KMAX )   !! virtual temperature
*
*   [INPUT]
      REAL*8     GDT   ( IJSDIM, KMAX       )  !! temperature T
      REAL*8     GDQ   ( IJSDIM, KMAX, NTR  )  !! q, cloud water
*
*   [INTERNAL WORK]
      INTEGER    IJ, K
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PMISC: VIRTUAL TEMP. 93/11/08'
         OFIRST = .FALSE.
      ENDIF
*
      IF ( ITL .GE. 2 ) THEN
         DO 1100 K = 1, KMAX
            DO 1100 IJ = 1, IJSDIM
               GDTV( IJ,K ) = GDT( IJ,K )
     &                        *( 1.+ EPSVT*GDQ( IJ,K,1   ) 
     &                             -       GDQ( IJ,K,ITL ) )
 1100    CONTINUE
      ELSE
         DO 1200 K = 1, KMAX
            DO 1200 IJ = 1, IJSDIM
               GDTV( IJ,K ) = GDT( IJ,K )
     &                        *( 1.+ EPSVT*GDQ( IJ,K,1 ) )
 1200    CONTINUE
      ENDIF
*
      RETURN
      END
**********************************************************************
      SUBROUTINE TINTP     !! vertical interpolation of temperature
     O         ( GDTM  ,
     I           GDT     )
*
*   [PARAM] 
#include        "zcdim.F"                 /* # of grid point & wave */
#include        "zcord.F"                /* coordinate              */
*
*   [OUTPUT] 
      REAL*8     GDTM  ( IJSDIM, KMAX+1 )     !! temp. (half lev)
*
*   [INPUT] 
      REAL*8     GDT   ( IJSDIM, KMAX )       !! temperature T
*
*   [INTERNAL WORK] 
      REAL*8     FTINT ( KMAX )                !! intrp. coef.
      REAL*8     FTINTM( KMAX )                !! intrp. coef.
*
      INTEGER    IJ, K
*      
*
*          < 1. intrp. fact. for T >
*
      DO 100 K = 2, KMAX
         FTINTM( K ) = (  LOG( SIGM( K )  ) - LOG( SIG( K )  )  )
     &               / (  LOG( SIG( K-1 ) ) - LOG( SIG( K )  )  )
         FTINT ( K ) = (  LOG( SIG( K-1 ) ) - LOG( SIGM( K ) )  )
     &               / (  LOG( SIG( K-1 ) ) - LOG( SIG( K )  )  )
  100 CONTINUE
*
*          < 2. interp. temp. >
*
      DO 200 K = 2, KMAX
         DO 200 IJ = 1, IJSDIM
            GDTM ( IJ,K ) = FTINTM( K ) * GDT ( IJ,K-1 )
     &                    + FTINT ( K ) * GDT ( IJ,K   )
  200 CONTINUE
*
      DO 210 IJ = 1, IJSDIM
            GDTM ( IJ,KMAX+1 ) = GDT ( IJ,KMAX )
            GDTM ( IJ,1      ) = GDT ( IJ,1 )
  210 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE PLEVS     !! calculate pressure
     O         ( GDP   , GDPM ,
     I           GDPS           )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     GDP    ( IJSDIM, KMAX )   !! pressure P
      REAL*8     GDPM   ( IJSDIM, KMAX+1 ) !! pressure (half lev)
*
*   [INPUT] 
      REAL*8     GDPS   ( IJSDIM         ) !! surface pressure Ps
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
*      < pressure(in Pa) >
*
      DO 1100 K = 1, KMAX
         DO 1110 IJ = 1, IJSDIM
            GDP ( IJ,K ) = GDPS( IJ ) * SIG ( K ) * 100.  
 1110    CONTINUE
 1100 CONTINUE
*
      DO 1200 K = 1, KMAX+1
         DO 1210 IJ = 1, IJSDIM
            GDPM( IJ,K ) = GDPS( IJ ) * SIGM( K ) * 100.  
 1210    CONTINUE
 1200 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE ZLEVS     !! calculate altitude
     O         ( GDZ   , GDZM  ,
     I           GDTV  , GDTVM , GDZS  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     GDZ    ( IJSDIM, KMAX )      !! altitude
      REAL*8     GDZM   ( IJSDIM, KMAX+1 )    !! altitude (half lev)
*
*   [INPUT] 
      REAL*8     GDTV   ( IJSDIM, KMAX   )    !! virtual temperature Tv
      REAL*8     GDTVM  ( IJSDIM, KMAX+1 )    !! virt. temp.(half lev)
      REAL*8     GDZS   ( IJSDIM )            !! surface topography
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
      DO 2100 IJ = 1, IJSDIM
         GDZ ( IJ,1 ) = GDZS( IJ ) 
     &                + RAIR * GDTV( IJ,1 ) / GRAV * ( 1. -SIG(1) )
 2100 CONTINUE
      DO 2200 K = 2, KMAX
         DO 2210 IJ = 1, IJSDIM
            GDZ ( IJ,K ) = GDZ ( IJ,K-1 )
     &                   + RAIR / GRAV * GDTVM( IJ,K )
     &                     * DSIGM( K )   / SIGM( K )
 2210    CONTINUE
 2200 CONTINUE
*
      CALL COPY( GDZM, GDZS, IJSDIM )
      DO 2300 K = 2, KMAX+1
         DO 2310 IJ = 1, IJSDIM
            GDZM( IJ,K ) = GDZM( IJ,K-1 )
     &                   + RAIR / GRAV * GDTV ( IJ,K-1 )
     &                     * DSIG ( K-1 ) / SIG ( K-1 )
 2310    CONTINUE
 2300 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE GDINTG    !! time integration in grid
     M         ( GDATA ,
     I           GTEND , DELT  , KDIMD )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
      INTEGER    KDIMD
*
*   [MODIFY] 
      REAL*8     GDATA ( IJSDIM, KDIMD )
*
*   [INPUT] 
      REAL*8     GTEND ( IJSDIM, KDIMD )
      REAL*8     DELT
*
*   [INTERNAL WORK] 
      INTEGER    I, IJ, K
*
      DO 1100 K = 1, KDIMD
         DO 1110 IJ = 0, IJSDIM-1, IDIM
            DO 1120 I = 1, IMAX
               GDATA( IJ+I,K ) = GDATA( IJ+I,K ) 
     &                         + DELT*GTEND( IJ+I,K )
 1120       CONTINUE 
 1110    CONTINUE 
 1100 CONTINUE 
*
      RETURN
      END
