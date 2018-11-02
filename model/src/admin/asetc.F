* PACKAGE ASETC  !! admin: constants (grid point)
*
*   [HIS] 96/06/21(numaguti) AGCM5.4f
***********************************************************************
      SUBROUTINE ASETCO    !!  coordinates of model
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zhdim.F"                /* # of character         */
#include        "zcord.F"                /* coordinate             */
#include        "zcnam.F"                /* coordinate name        */
*
      CALL SETLON
     O         ( ALON  , DLON , HALON  )
      CALL SETLAT
     O         ( ALAT  , DLAT , HALAT  )
      CALL SETSIG
     O         ( SIG   , DSIG , HSIG  , 
     O           SIGM  , DSIGM, HSIGM  )
*
      RETURN
      END
************************************************************************
      SUBROUTINE SETLON    !!  longitude coordinate
     O         ( ALON  , DLON  , HALON  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zhdim.F"
*
*   [OUTPUT] 
      REAL*8     ALON  ( IDIM, JDIM )         !! longitude
      REAL*8     DLON  ( IDIM, JDIM )         !! longitudinal differ.
      CHARACTER  HALON *(NCC)
*
*   [WORK] 
      INTEGER    I, J
      REAL*8     PI
*
      PI = ATAN( 1.D0 ) *4.D0
      CALL RESET ( ALON, IDIM*JDIM )
      CALL RESET ( DLON, IDIM*JDIM )
* 
      CALL SETLO          !!  longitude coordinate
     O         ( ALON  , DLON  , HALON, 
     I           IMAX  , MINT           )
*
      DO 1100 J = 1, JDIM
         DO 1110 I = 1, IMAX
            ALON( I,J ) = ALON( I,1 )
            DLON( I,J ) = DLON( I,1 )
 1110    CONTINUE 
         DO 1120 I = IMAX+1, IDIM
            ALON( I,J ) = ALON( I-IMAX,1 ) + PI*2.D0
            DLON( I,J ) = 0.D0
 1120    CONTINUE 
 1100 CONTINUE 
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SETLAT    !!  latitude coordinate
     O         ( ALAT  , DLAT  , HALAT   )
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zhdim.F"
*
*   [OUTPUT] 
      REAL*8     ALAT  ( IDIM,JDIM )              !! latitude
      REAL*8     DLAT  ( IDIM,JDIM )              !! weight of latitude
      CHARACTER  HALAT *(NCC)
*
*    [INTERNAL WORK]
      REAL * 8   ALAT1 ( JDIM )             !! sin(lat.):double
      REAL * 8   DLAT1 ( JDIM )             !! Gaussian weight:double
*
*   [WORK] 
      INTEGER    I, J
*
      CALL RESET ( ALAT, IDIM*JDIM )
      CALL RESET ( DLAT, IDIM*JDIM )
* 
      CALL SETLA     !!  latitude coordinate
     O         ( ALAT1 , DLAT1 , HALAT ,
     I           JMAX                    )
*
      DO 1100 I = 1, IDIM
         DO 1110 J = 1, JMAX
            ALAT( I,J ) = ALAT1( J )
            DLAT( I,J ) = DLAT1( J )
 1110    CONTINUE 
         DO 1120 J = JMAX+1, JDIM
            ALAT( I,J ) = ALAT1( JMAX )
            DLAT( I,J ) = 0.D0
 1120    CONTINUE 
 1100 CONTINUE 
*
      RETURN
      END
************************************************************************
      SUBROUTINE SETLO     !!  longitude coordinate
     O         ( ALON  , DLON  , HALON ,
     I           IMAX  , MINT           )
*
*   [PARAM] 
#include        "zhdim.F"
      INTEGER    IMAX
      INTEGER    MINT
*
*   [OUTPUT] 
      REAL*8     ALON  ( IMAX )             !! longitude
      REAL*8     DLON  ( IMAX )             !! longitudinal differ.
      CHARACTER  HALON *(NCC)
*
*    [INTERNAL WORK]
      INTEGER    I, IFPAR, JFPAR
      REAL*8     PI
*
*    [INTERNAL PARAM]
      REAL*8     ALON0D                     !! constant longitude (deg.)
      CHARACTER  HALON0 *(NCC)              !! name of longitude axis
      NAMELIST  /NMLON0/ ALON0D, HALON0
      DATA       ALON0D / -999. /
      DATA       HALON0 / 'GLON' /
*
      IF ( IMAX .LE. 2 ) THEN
         WRITE ( 6,* ) ' CONTROL PARAMETER NMLON0: '
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMLON0, END=1090 )
 1090    WRITE  ( JFPAR, NMLON0 )
      ENDIF
*
      PI = ATAN( 1.D0 ) *4.D0
      DO 1100 I = 1, IMAX
         IF ( ALON0D .GE. 0. ) THEN
            ALON( I ) = ALON0D*PI/180.D0
         ELSE
            ALON( I ) = DBLE( I-1 )/DBLE( IMAX*MINT ) * 2.D0*PI
         ENDIF
         DLON   ( I ) = 1. / DBLE( IMAX*MINT )
 1100 CONTINUE
*
      CALL CATNUM( HALON, HALON0, IMAX, '(I4)' )
      IF ( MINT .NE. 1 ) THEN
         HALON0 = HALON
         CALL CATNUM( HALON, HALON0, MINT, '(1HSI1)' )
      ENDIF
      IF ( IMAX .EQ. 1 ) HALON = ' ' 
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SETLA     !!  latitude coordinate
     O         ( ALAT  , DLAT  , HALAT  ,
     I           JMAX                    )
*   [PARAM] 
#include        "zhdim.F"
      INTEGER    JMAX
*
*   [OUTPUT] 
      REAL * 8   ALAT  ( JMAX )            !! latitude
      REAL * 8   DLAT  ( JMAX )            !! weight of latitude
      CHARACTER  HALAT *(NCC)
*
*    [INTERNAL WORK]
      INTEGER    IFPAR, JFPAR, J
      REAL*8     PI, SUMGW
*
*    [INTERNAL PARAM]
      REAL*8     ALAT0D                       !! when JMAX=1(degree)
      CHARACTER  HALAT0 *(NCC)                !! name of latitude axis
      NAMELIST  /NMLAT0/ ALAT0D, HALAT0
      DATA       ALAT0D / 0.0  /
      DATA       HALAT0 / 'GGLA' /
*    
      IF ( JMAX .GE. 2 ) THEN
         CALL GAUSS
     O         ( ALAT  , DLAT   ,
     I           JMAX             )
*
         SUMGW = 0.D0  
         DO 1100 J = 1, JMAX
            ALAT( J ) = ASIN( ALAT( J ) )
            SUMGW = SUMGW + DLAT( J )
 1100    CONTINUE
         WRITE ( 6,* ) ' ### check SUMGW = ', SUMGW
      ELSE
         WRITE ( 6,* ) ' CONTROL PARAMETER NMLAT0: '
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMLAT0, END=1090 )
 1090    WRITE  ( JFPAR, NMLAT0 )
         PI = ATAN( 1.D0 )*4.D0
         ALAT( 1 ) = ALAT0D/180.D0*PI
         DLAT( 1 ) = 1.D0
      ENDIF
*
      CALL CATNUM( HALAT, HALAT0, JMAX, '(I4)' )
      IF ( JMAX .EQ. 1 ) HALAT = ' ' 
*
      RETURN
      END
