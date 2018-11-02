* PACKAGE DSTEP !! dynamics: integration of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         00/04/10(hiro)     planetary wave model
*         01/06/18(hiro)     barotropic model
**********************************************************************
      SUBROUTINE DYNSTP    !! time step of dynamics
     M         ( WTVOR , WDVOR ,
     I           DELT  , OINIT , DIFV  , NOMWV    )
*     
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* physics etc. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WDVOR ( NMDIM, KMAX )       !! vorticity
*
*   [INPUT] 
      REAL*8     DELT                        !! time step
      LOGICAL    OINIT                       !! initialized condition ?
      REAL*8     DIFV  ( NMDIM, KMAX )       !! h-diff. coef (vor)

      INTEGER    NOMWV ( NMDIM )
*
*   [WORK] 
      REAL*8     GDX   ( IJDIM, KMAX )
*
*   [INTERNAL WORK] 
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST          / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSTEP: DYNAMICAL STEP 00/04/12'
         OFIRST = .FALSE.
      ENDIF
*
*  << INTGR : time integration >>
*
      CALL TINTGR                      !! spectral time integration
     M         ( WDVOR , 
     M           WTVOR , 
     I           DELT  , NOMWV , DIFV )
      CALL W2G
     O     ( GDX   ,
     I       WTVOR ,
     I       '    ', 'POS ', KMAX )
cc      call wrt45( gdx, imax, idim, jdim )
*
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine wrt45( x, id, jd )
      integer id,jd,i,j
      real*8 x( id,jd )
      open( 45, file='/home/tropics/hiro/ln_solver/matrix/out/ft45.grd',
     &     form='unformatted' )
      write(45) ((sngl(x(i,j)),i=1,id-1),j=1,jd)
      return
      end
***********************************************************************
      SUBROUTINE TINTGR    !! time integration (spectral)
     M         ( WDVOR , 
     M           WTVOR , 
     I           DELT  , NOMWV , DIFV )
*
*                    implicit horizontal diffusion
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY] 
      REAL*8     WDVOR ( NMDIM , KMAX     )  !! vorticity
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
*
*   [INPUT] 
      REAL*8     DELT                        !! time step delta(t)
      REAL*8     DIFV  ( NMDIM, KMAX )       !! h-diff. coef (vor)
      INTEGER    NOMWV ( NMDIM )
*
*   [INTERNAL WORK] 
      REAL*8     DELT2
      REAL*8     WV                          !! temporary for WDVOR
      INTEGER    NM, K
      INTEGER    IFPAR, JFPAR
*     
*   [INTERNAL SAVE] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      SAVE       NMO
*
*   [INTERNAL PARAM]
      REAL*8     DIST0
      REAL*8     DSEC
      REAL*8     DISTV ( KMAX )              !! linear drag 
      REAL*8     ZMFCTV( KMAX )              !! factor for zonal mean vor.
      REAL*8     DDRAGV( KMAX )              !! damping time scale (dy)
      REAL*8     S2D                         !! second in 1 day
      CHARACTER  TUNIT*4                     !! unit of time
      LOGICAL    OFIRST

      NAMELIST  /NMDAMP/  DDRAGV, TUNIT
      NAMELIST  /NMZMFCT/ ZMFCTV

      DATA       S2D     / 86400.D0   /
      DATA       DSEC    / 0.D0       /
      DATA       OFIRST  / .TRUE.     /
      DATA       DDRAGV  / KMAX*-1.D0 /
      DATA       ZMFCTV  / KMAX*1.D0  /
      DATA       TUNIT   / 'DAY'      /

*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DINTG: TIME INTEGRATION 01/06/15'
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMDAMP, END=90 )
   90    WRITE  ( JFPAR, NMDAMP )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMZMFCT, END=91 )
   91    WRITE  ( JFPAR, NMZMFCT )
*
         CALL DSETNM
     O         ( NMO   )
*
         CALL RESET( DISTV, KMAX )
         DO 1 K = 1, KMAX
            DIST0 = DDRAGV( K )
            CALL CXX2SS ( DISTV( K ), DIST0, TUNIT, DSEC  )
            IF( DISTV( K ) .LE. 0 ) THEN
               DISTV( K ) = 0.D0
            ELSE
               DISTV( K ) = 1.D0 / DISTV( K )
            ENDIF
    1    CONTINUE
*
         DO 1100  K = 1 , KMAX
            DO 1100  NM = 1 , NMDIM
               DIFV( NM, K) = DIFV( NM, K) + DISTV( K )
 1100    CONTINUE 
         DO 1200 K = 1, KMAX
            DO 1300 NM = 1, LMAX
               difv(nmo(1,0,nm),k) = 
     &              difv(nmo(1,0,nm),k)-distv(k)*(1.D0-ZMFCTV(K))
cc               difv(nmo(1,0,nm),k) = 
cc     &              difv(nmo(1,0,nm),k)
cc     &              *0.d0
cc     &              -distv(k)*0.8
cc     &              +distv(k)*10.d0
cc     &              + 2.d0 / s2d !! if necessary, 2001/07/03
cc               if( nm.le.2 ) difv(nmo(1,0,nm),k) = 
cc     &              difv(nmo(1,0,3),k)
 1300       CONTINUE
 1200    CONTINUE
*
#ifdef OPT_WVFRC
         CALL RESET( DIFV, NMDIM*KMAX )
#endif
*
         OFIRST = .FALSE.
      ENDIF
*
      DELT2  = DELT*2.D0  
*
*          <time integration >
*
      DO 6100  K = 1 , KMAX
         DO 6100  NM = 1 , NMDIM
            IF( NOMWV( NM ) .NE. 1 ) GOTO 6100
            WV            = ( WDVOR( NM,K ) + WTVOR( NM,K )*DELT2 )
     &                     / ( 1.D0 + DELT2*DIFV( NM,K )          )
            WTVOR( NM, K) = ( WV - WDVOR( NM, K ) ) / DELT2
            WDVOR( NM, K ) = WV
*
 6100 CONTINUE
*
*
      RETURN
      END
