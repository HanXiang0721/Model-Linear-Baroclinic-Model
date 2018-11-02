* PACKAGE IHSUB     !! history package
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/12/04(numaguti) PSMLT
*         97/08/01(numaguti) MOSAIC
************************************************************************
      SUBROUTINE HSTADD    !! add into history output
     M         ( GOUT  , INDUR ,
     I           GDATA , RMULT , RMISS , 
     I           OPOUT , OPSMLT, IDIUR , DELT  , OSQU  ,
     I           HCORD , IXSEL , IYSEL , IZSEL , IMSEL )
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zidim.F"                /* number of output data */
#include        "zhdim.F"                /* No. of character */
#include        "zmdim.F"                /* No. of mosaic */
*
*   [MODIFY] 
      REAL*8     GOUT   ( * )                !! output buffer
      INTEGER    INDUR  ( * )                !! # of added data
*
*   [INPUT] 
      REAL*8     GDATA  ( * )                !! data
      REAL*8     RMULT                       !! fact.
      REAL*8     RMISS                       !! missing val.
      LOGICAL    OPOUT                       !! p-lev output
      LOGICAL    OPSMLT                      !! multiply Ps
      INTEGER    IDIUR                       !! diurnal out
      REAL*8     DELT                        !! time step
      LOGICAL    OSQU                        !! square output
      CHARACTER  HCORD *(*)                  !! axis coordinates
      INTEGER    IXSEL                       !! x selection
      INTEGER    IYSEL                       !! y selection
      INTEGER    IZSEL                       !! z selection
      INTEGER    IMSEL                       !! mozaic selection
*
*   [ENTRY INPUT] 
      INTEGER    IMOZZ                       !! mosaic weight
      REAL*8     FMOZ   ( IJSDIM )           !! mosaic weight
*
*   [INTERNAL WORK] 
      REAL*8     GDATAW ( NGDWRK )           !! work
      INTEGER    IDIMS , JDIMS
      INTEGER    IDIME , JDIME
      INTEGER    IMAXD , JMAXD , KMAXD, KMAXI
      INTEGER    IDIMD , JDIMD
      INTEGER    ISIZE, IJK, IJDIMD, IJ, K, IJKS
      CHARACTER  HCORDY *(NCC)
      REAL*8     TOTW
      REAL*8     RMULZ
      REAL*8     DELTZ
*
*   [INTERNAL SAVE] 
      REAL*8     WMOZ   ( IJSDIM )           !! mosaic weight
      SAVE       WMOZ 
      INTEGER    IMOZS
      LOGICAL    OFIRST
      DATA       IMOZS  / 0       /
      DATA       OFIRST / .TRUE.  /
*
      IF ( OFIRST ) THEN
         WRITE (6,*) ' @@@ IHSUB: HISTORY SUMMING UP 96/12/04'
         OFIRST = .FALSE.
      ENDIF
*
      CALL       HQCSLX( IDIMS, IDIME, HCORD )
      CALL       HQCSLY( JDIMS, JDIME, HCORD )
      CALL       HQCDMX( IMAXD, IDIMD, HCORD )
      CALL       HQCDMY( JMAXD, JDIMD, HCORD )
      CALL       HQCDMZ( KMAXD,        HCORD )
      HCORDY = HCORD
      ISIZE  = (IDIME-IDIMS+1)*(JDIME-JDIMS+1)*KMAXD
*
      IF ( HCORD(1:1) .EQ.'A' ) THEN
         IF ( OPOUT .AND. KMAXD .GT.1 ) THEN
            KMAXI = KMAXD
            CALL S2PINT
     O      ( GDATAW, KMAXD ,
     I        GDATA , KMAXI , HCORD , RMISS ,
     O        IDIMS , JDIMS , IDIME , JDIME   )
            HCORDY = 'APLEV'
         ELSE IF ( OPSMLT ) THEN
            CALL SPSMLT
     O      ( GDATAW, 
     I        GDATA , KMAXD , 
     O        IDIMS , JDIMS , IDIME , JDIME   )
         ELSE
            CALL COPY( GDATAW, GDATA, ISIZE )
         ENDIF
      ELSE
         CALL COPY( GDATAW, GDATA, ISIZE )
      ENDIF
      IF ( OSQU ) THEN
         ISIZE = (IDIME-IDIMS+1)*(JDIME-JDIMS+1)*KMAXD
         DO 2100 IJK = 1, ISIZE
            IF ( GDATAW( IJK ) .NE. RMISS ) THEN
               GDATAW( IJK ) = GDATAW( IJK )**2
            ENDIF
 2100    CONTINUE 
      ENDIF
*
      RMULZ = RMULT
      DELTZ = DELT
*
      IF ( IMOZS .GT. 0 ) THEN
         IF ( IMSEL .EQ. 0 ) THEN 
            IJDIMD = (IDIME-IDIMS+1)*(JDIME-JDIMS+1)
            DO 3100 K = 1, KMAXD
               IJKS = (K-1)*IJDIMD
               DO 3100 IJ = 1, IJDIMD
                  IJK = IJ + IJKS
                  GDATAW( IJK ) = GDATAW( IJK )*WMOZ( IJ )
 3100       CONTINUE
            IF ( IMOZS .GT. 1 ) DELTZ = 0.D0
         ELSE IF ( IMSEL .NE. IMOZS ) THEN
            RETURN
         ENDIF
      ENDIF
*
      IF      ( IXSEL .EQ. 0 ) THEN
         CALL XAVR
     M        ( GDATAW, IMAXD , JMAXD , KMAXD ,
     O          TOTW  ,
     I          HCORDY, RMISS ,
     I          IDIMS , JDIMS , IDIME , JDIME  )
         RMULZ = RMULZ*TOTW
         IF ( IDIMS .GT. 1 ) DELTZ = 0.D0
      ELSE IF ( IXSEL .GT. 0 ) THEN
         CALL XSEL
     M        ( GDATAW, IMAXD , JMAXD , KMAXD ,
     I          IXSEL , RMISS , 
     I          IDIMS , JDIMS , IDIME , JDIME  )
      ENDIF
*
      IF      ( IYSEL .EQ. 0 ) THEN
         CALL YAVR
     M        ( GDATAW, IMAXD , JMAXD , KMAXD ,
     O          TOTW  ,
     I          HCORDY, RMISS ,
     I          IDIMS , JDIMS , IDIME , JDIME  )
         RMULZ = RMULZ*TOTW
         IF ( JDIMS .GT. 1 ) DELTZ = 0.D0
      ELSE IF ( IYSEL .GT. 0 ) THEN
         CALL YSEL
     M        ( GDATAW, IMAXD , JMAXD , KMAXD ,
     I          IYSEL , RMISS ,
     I          IDIMS , JDIMS , IDIME , JDIME  )
      ENDIF
*
      IF      ( IZSEL .EQ. 0 ) THEN
         CALL ZAVR
     M        ( GDATAW, IMAXD , JMAXD , KMAXD ,
     I          HCORDY, RMISS ,
     I          IDIMS , JDIMS , IDIME , JDIME  )
      ELSE IF ( IZSEL .GT. 0 ) THEN
         CALL ZSEL
     M        ( GDATAW, IMAXD , JMAXD , KMAXD ,
     I          IZSEL , RMISS ,
     I          IDIMS , JDIMS , IDIME , JDIME  )
      ENDIF
*
      IF ( IDIUR .NE. 0 .AND. KMAXD .EQ. 1 ) THEN
         CALL DIURAV
     M        ( GDATAW, IMAXD , JMAXD , KMAXD ,
     I          IDIUR , IXSEL , HCORDY, RMISS , 
     I          IDIMS , JDIMS , IDIME , JDIME  )
      ENDIF
*               
      CALL HSTADX
     M        ( GOUT  , INDUR ,
     I          GDATAW, IMAXD , JMAXD , KMAXD, 
     I          RMULZ , DELTZ , RMISS ,
     I          IDIMS , JDIMS , IDIME , JDIME  )
*
      RETURN
*======================================================================
      ENTRY      HSTMOZ    !! set history mosaic spec
     I         ( IMOZZ , FMOZ   )
*
      IMOZS = IMOZZ
      IF ( IMOZ .GT. 0 ) THEN
         CALL COPY( WMOZ, FMOZ, IJSDIM )
      ENDIF
*
      RETURN
      END
***********************************************************************
      SUBROUTINE HSTADX    !! add into matrix
     M         ( GOUT  , INDUR ,
     I           GDATA , IMAXD , JMAXD , KMAXD , 
     I           RMULT , DELT  , RMISS ,        
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
*   [PARAM] 
      INTEGER    IMAXD, JMAXD, KMAXD
      INTEGER    IDIMS, IDIME
      INTEGER    JDIMS, JDIME
*
*   [MODIFY] 
      REAL*8     GOUT  ( IMAXD, JMAXD, KMAXD )
      INTEGER    INDUR ( IMAXD, JMAXD, KMAXD )
*
*   [INPUT] 
      REAL*8     GDATA ( IDIMS:IDIME, JDIMS:JDIME, * )
      REAL*8     RMULT
      REAL*8     DELT
      REAL*8     RMISS
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K
      INTEGER    IMAXE, JMAXE
*
      IMAXE = MIN( IMAXD,IDIME )
      JMAXE = MIN( JMAXD,JDIME )
*
      IF ( RMISS .NE. 0. ) THEN
         DO 1100 K = 1, KMAXD
            DO 1100 J = JDIMS, JMAXE
               DO 1100 I = IDIMS, IMAXE
                  IF ( GDATA( I,J,K ) .NE. RMISS ) THEN
                     GOUT( I,J,K ) = GOUT( I,J,K )+GDATA( I,J,K )*RMULT
                    INDUR( I,J,K ) = INDUR( I,J,K ) + NINT( DELT )
                  ENDIF
 1100    CONTINUE 
      ELSE
         DO 1200 K = 1, KMAXD
            DO 1200 J = JDIMS, JMAXE
               DO 1200 I = IDIMS, IMAXE
                  GOUT( I,J,K ) = GOUT( I,J,K ) + GDATA( I,J,K )*RMULT
                 INDUR( I,J,K ) = INDUR( I,J,K ) + NINT( DELT )
 1200    CONTINUE 
      ENDIF
*
      RETURN
      END
***********************************************************************
      SUBROUTINE RDATF0    !! time interpolation of input data
     O         ( GDATAW,
     I           F0    , GIN0  , GIN1  ,     
     I           IMAXDR, JMAXDR, KMAXDR,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
*   [PARAM] 
      INTEGER    IDIMS, IDIME
      INTEGER    JDIMS, JDIME
      INTEGER    IMAXDR, JMAXDR, KMAXDR
*
*   [OUTPUT] 
      REAL*8     GDATAW ( IDIMS:IDIME, JDIMS:JDIME, * )
*
*   [INPUT] 
      REAL*8     F0
      REAL*8     GIN0    ( IMAXDR, JMAXDR, KMAXDR )
      REAL*8     GIN1    ( IMAXDR, JMAXDR, KMAXDR )
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K
      INTEGER    II, JJ
*
      DO 1100 K = 1, KMAXDR
         DO 1100 J = JDIMS, JDIME
            DO 1100 I = IDIMS, IDIME
               II = MIN( I, IMAXDR )
               JJ = MIN( J, JMAXDR )
               GDATAW( I,J,K ) = GIN0( II,JJ,K ) * F0
     &                         + GIN1( II,JJ,K ) * (1.-F0)
 1100 CONTINUE 
*
      RETURN
      END
***********************************************************************
      SUBROUTINE HISIZE    !! size of history data area
     O         ( IMAXD , JMAXD , KMAXD ,
     O           HAX   , HAY   , HAZ   ,
     I           HCORD , IXSEL , IYSEL , IZSEL , 
     I           IMSEL , OPOUT , IDIUR                    )
*
#include        "zhdim.F"                /* No. of character */
*
*   [OUTPUT] 
      INTEGER    IMAXD
      INTEGER    JMAXD
      INTEGER    KMAXD
      CHARACTER  HAX   *(*)
      CHARACTER  HAY   *(*)
      CHARACTER  HAZ   *(*)
*
*   [INPUT] 
      CHARACTER  HCORD *(*)                   !! coordinate
      INTEGER    IXSEL                        !! selected type of X
      INTEGER    IYSEL                        !! selected type of Y
      INTEGER    IZSEL                        !! selected type of Z
      INTEGER    IMSEL                        !! selected type of MOZ
      LOGICAL    OPOUT                        !! output at P-level
      INTEGER    IDIUR                        !! intv. of diurnal var.
*
*   [INTERNAL WORK] 
      CHARACTER  HAXIS  *(NCC)
      CHARACTER  HCORDY *(NCC)
      REAL*8     PX, PY, PZ
      INTEGER    IDIMD, JDIMD
      INTEGER    NSECDY, NSECHR
*
*   [INTERNAL PARM] 
      CHARACTER  HFMT *(NCC)
      DATA       HFMT / '(G10.4)' /
*
      IF ( OPOUT .AND. ( HCORD.EQ.'ALEV' .OR. HCORD.EQ.'AMLEV' ) ) THEN
         HCORDY = 'APLEV'
      ELSE
         HCORDY = HCORD
      ENDIF
*
      CALL HQCDMX( IMAXD , IDIMD , HCORDY )
      CALL HQCDMY( JMAXD , JDIMD , HCORDY )
      CALL HQCDMZ( KMAXD ,         HCORDY )
      CALL HQCNMX( HAX, HCORDY )
      CALL HQCNMY( HAY, HCORDY )
      CALL HQCNMZ( HAZ, HCORDY )
*      
      IF      ( IXSEL .EQ. 0 ) THEN
         HAX   = '=AV'
         IMAXD = 1
      ELSE IF ( IXSEL .GT. 0 ) THEN
         CALL HQCAXX( PX, HCORDY, IXSEL, 1 )
         WRITE( HAXIS,HFMT ) PX  
         CALL CLADJ( HAXIS )
         HAX   = '='//HAXIS
         IMAXD = 1
      ENDIF
*
      IF      ( IYSEL .EQ. 0 ) THEN
         HAY   = '=AV'
         JMAXD = 1
      ELSE IF ( IYSEL .GT. 0 ) THEN
         CALL HQCAXY( PY, HCORDY, IYSEL, 1 )
         WRITE( HAXIS,HFMT ) PY  
         CALL CLADJ( HAXIS )
         HAY   = '='//HAXIS
         JMAXD = 1
      ENDIF
*
      IF      ( IZSEL .EQ. 0 ) THEN
         HAZ   = '=AV'
         KMAXD = 1
      ELSE IF ( IZSEL .GT. 0 ) THEN
         CALL HQCAXZ( PZ, HCORDY, IZSEL, 1 )
         WRITE( HAXIS,HFMT ) PZ  
         CALL CLADJ( HAXIS )
         HAZ   = '='//HAXIS
         KMAXD = 1
      ENDIF
*
      IF ( KMAXD .EQ. 1 .AND. IDIUR .NE. 0 ) THEN
         CALL CSECDY( NSECDY )
         CALL CSECHR( NSECHR )
         KMAXD = MAX(NSECDY/(NSECHR*ABS(IDIUR)),1)
         HAXIS = 'DIUR'
         WRITE (HAXIS(5:6),'(I2.2)') KMAXD
         HAZ   = HAXIS
      ENDIF
*
      RETURN
      END
