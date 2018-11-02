* PACKAGE ICORD !! set coordinates for history data
*
*   [HIS] 96/06/21(numaguti) AGCM5.4f
*         97/08/04(numaguti) AGCM5.4h
***********************************************************************
      SUBROUTINE HQCORD    !! get coordinate
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zidim.F"                /* number of output data  */
#include        "zhdim.F"                /* No. of character       */
#include        "zcord.F"                /* atmospheric coordinate */
#include        "zpdim.F"                /* physics etc.           */
#include        "zcorg.F"                /* land        coordinate */
#include        "zcnam.F"                /* coordinate name        */
*
*
*   [INPUT] 
*
*   [ENTRY OUTPUT] 
      CHARACTER  HAX    *(*)                 !! name of axis
      INTEGER    MAXD
      INTEGER    DIMD
      REAL*8     AX ( * )
      REAL*8     DX ( * )
*   [ENTRY INPUT] 
      CHARACTER  HCORD  *(*)                 !! axis coordinates
      INTEGER    ISEL
      INTEGER    INUM
*
*   [WORK] 
      INTEGER    I, K, IFPAR, JFPAR
*
*   [INTERNAL PARM] 
      INTEGER    KPLEV                       !! No. of P-level
      REAL*8     PLEV  ( KPDIM )             !! P-level
      CHARACTER  HPLEV *(NCC)                !! name of P-level
      REAL*8     DPLEV ( KPDIM )
      SAVE       DPLEV
      LOGICAL    OFIRST 
      NAMELIST  /NMPLEV/  KPLEV, PLEV, HPLEV
      DATA       KPLEV  / 17 /
      DATA       PLEV   / 1000., 950., 900., 850., 700., 500., 
     &                     400., 300., 250., 200., 150., 100.,
     &                      70.,  50.,  30.,  20.,  10., 0.    /
      DATA       HPLEV  / 'STDPL17' /
      DATA       OFIRST / .TRUE. /
*
*=====================================================================
      ENTRY      HQCNMX     !! get atmos coordinate name
     O         ( HAX    ,
     I           HCORD   )
*
      HAX = HALON
      RETURN
*=====================================================================
      ENTRY      HQCNMY     !! get atmos coordinate name
     O         ( HAX    ,
     I           HCORD   )
*
      HAX = HALAT
      RETURN
*=====================================================================
      ENTRY      HQCNMZ     !! get atmos coordinate name
     O         ( HAX    ,
     I           HCORD   )
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE. 
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMPLEV, END=190 )
  190    WRITE  ( JFPAR, NMPLEV )
         DO 200 K = 1, KPLEV
            DPLEV( K ) = ( PLEV(MAX(K-1,1))-PLEV(MIN(K+1,KPLEV)) )/2.
  200    CONTINUE 
      ENDIF
*
      IF      ( HCORD .EQ. 'ASFC'  ) THEN
         HAX = 'SFC1'
      ELSE IF ( HCORD .EQ. 'ALEV'  ) THEN
         HAX = HSIG
      ELSE IF ( HCORD .EQ. 'AMLEV' ) THEN
         HAX = HSIGM
      ELSE IF ( HCORD .EQ. 'GLEV'  ) THEN
         HAX = HGLEV
      ELSE IF ( HCORD .EQ. 'GMLEV' ) THEN
         HAX = HGLEVM
      ELSE IF ( HCORD .EQ. 'WLEV'  ) THEN
         HAX = HWLEV
      ELSE IF ( HCORD .EQ. 'WMLEV' ) THEN
         HAX = HWLEVM
      ELSE IF ( HCORD .EQ. 'APLEV' ) THEN
         HAX = HPLEV
      ELSE 
         HAX = 'NUMBER1000'
      ENDIF
*
      RETURN
*=====================================================================
      ENTRY      HQCDMX     !! get size of coordinates
     O         ( MAXD  , DIMD  ,
     I           HCORD          )
*
      MAXD = IMAX
      DIMD = IDIM
      RETURN
*=====================================================================
      ENTRY      HQCDMY     !! get size of coordinates
     O         ( MAXD  , DIMD  ,
     I           HCORD          )
*
      MAXD = JMAX
      DIMD = JDIM
      RETURN
*=====================================================================
      ENTRY      HQCDMZ     !! get size of coordinates
     O         ( MAXD ,
     I           HCORD  )
*
      IF      ( HCORD .EQ. 'ASFC'  ) THEN
         MAXD = 1
      ELSE IF ( HCORD .EQ. 'ALEV'  ) THEN
         MAXD = KMAX
      ELSE IF ( HCORD .EQ. 'AMLEV' ) THEN
         MAXD = KMAX+1
      ELSE IF ( HCORD .EQ. 'GLEV'  ) THEN
         MAXD = KGMAX
      ELSE IF ( HCORD .EQ. 'GMLEV' ) THEN
         MAXD = KGMAX+1
      ELSE IF ( HCORD .EQ. 'WLEV'  ) THEN
         MAXD = KWMAX
      ELSE IF ( HCORD .EQ. 'WMLEV' ) THEN
         MAXD = KWMAX+1
      ELSE IF ( HCORD .EQ. 'APLEV' ) THEN
         MAXD = KPLEV
      ELSE IF ( HCORD .EQ. 'GMOZ' ) THEN
         MAXD = NMOZ
      ELSE 
         READ( HCORD, '(I2)' ) MAXD
      ENDIF
*
      RETURN
*=====================================================================
      ENTRY      HQCAXX    !!  get coordinate points
     O         ( AX   , 
     I           HCORD, ISEL, INUM  )
*
      CALL COPY( AX, ALON(ISEL), INUM )      
      RETURN
*=====================================================================
      ENTRY      HQCAXY    !!  get coordinate points
     O         ( AX   , 
     I           HCORD, ISEL, INUM  )
*
      DO 3100 I = 1, INUM
         AX( I ) = ALAT( (I+ISEL-2)*IDIM + 1 )
 3100 CONTINUE 
      RETURN
*=====================================================================
      ENTRY      HQCAXZ    !!  get coordinate points
     O         ( AX   , 
     I           HCORD, ISEL, INUM  )
*
      IF      ( HCORD .EQ. 'ASFC'  ) THEN
         AX( 1 ) = 1.D0
      ELSE IF ( HCORD .EQ. 'ALEV'  ) THEN
         CALL COPY( AX, SIG  (ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'AMLEV' ) THEN
         CALL COPY( AX, SIGM (ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'GLEV'  ) THEN
         CALL COPY( AX, GLEV (ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'GMLEV' ) THEN
         CALL COPY( AX, GLEVM(ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'WLEV'  ) THEN
         CALL COPY( AX, WLEV (ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'WMLEV' ) THEN
         CALL COPY( AX, WLEVM(ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'APLEV' ) THEN
         CALL COPY( AX, PLEV (ISEL), INUM )
      ELSE 
         DO 3200 I = ISEL, ISEL+INUM-1
            AX(I) = I
 3200    CONTINUE 
      ENDIF
*
      RETURN
*=====================================================================
      ENTRY      HQCDXX    !!  get coordinate delta
     O         ( DX   ,
     I           HCORD, ISEL, INUM  )
*
      CALL COPY( DX, DLON(ISEL), INUM )      
      RETURN
*=====================================================================
      ENTRY      HQCDXY    !!  get coordinate delta
     O         ( DX   ,
     I           HCORD, ISEL, INUM  )
*
      DO 4100 I = 1, INUM
         DX( I ) = DLAT( (I+ISEL-2)*IDIM + 1 )
 4100 CONTINUE 
      RETURN
*=====================================================================
      ENTRY      HQCDXZ    !!  get coordinate delta
     O         ( DX   ,
     I           HCORD, ISEL, INUM  )
*
      IF      ( HCORD .EQ. 'ASFC'  ) THEN
         DX( 1 ) = 1.D0
      ELSE IF ( HCORD .EQ. 'ALEV'  ) THEN
         CALL COPY( DX, DSIG (ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'AMLEV' ) THEN
         CALL COPY( DX, DSIGM(ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'GLEV'  ) THEN
         CALL COPY( DX, DGLV (ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'GMLEV' ) THEN
         CALL COPY( DX, DGLVM(ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'WLEV'  ) THEN
         CALL COPY( DX, DWLV (ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'WMLEV' ) THEN
         CALL COPY( DX, DWLVM(ISEL), INUM )
      ELSE IF ( HCORD .EQ. 'APLEV' ) THEN
         CALL COPY( DX, DPLEV(ISEL), INUM )
      ELSE
         CALL SETV( DX, 1.D0, INUM )
      ENDIF
*
      RETURN
      END
******************************************************
      SUBROUTINE HQCSLX        !! get active region
     O         ( ISELS , ISELE , 
     I           HCORD          )
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zhdim.F"                /* No. of character       */
*
*   [OUTPUT] 
      INTEGER    ISELS
      INTEGER    ISELE
*
*   [INPUT] 
      CHARACTER  HCORD  *(*)                 !! axis coordinates
*
*   [INTERNAL SAVE]       
      INTEGER    IDIMSZ, IDIMEZ
      INTEGER    JDIMSZ, JDIMEZ
      DATA       IDIMSZ, IDIMEZ / 1, IDIM /
      DATA       JDIMSZ, JDIMEZ / 1, JDIM /
*
      ISELS = IDIMSZ
      ISELE = IDIMEZ
*
      RETURN
*=====================================================================
      ENTRY      HQCSLY         !! get active region
     O         ( ISELS , ISELE , 
     I           HCORD          )
*
      ISELS = JDIMSZ
      ISELE = JDIMEZ
*
      RETURN
*=====================================================================
      ENTRY      HSCSLX    !! set history data area
     I         ( ISELS , ISELE , HCORD )
*
      IDIMSZ = ISELS
      IDIMEZ = ISELE
*
      RETURN
*=====================================================================
      ENTRY      HSCSLY    !! set history data area
     I         ( ISELS , ISELE , HCORD )
*
      JDIMSZ = ISELS
      JDIMEZ = ISELE
*
      RETURN
      END
