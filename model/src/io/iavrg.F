* PACKAGE IAVRG !! areal average of history data
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         97/08/04(numaguti) AGCM5.4h
************************************************************************
      SUBROUTINE XAVR      !! longitudinal mean
     M         ( GDATA , IMAXD , JMAXD , KMAXD ,
     O           TOTLON,
     I           HCORD , RMISS ,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zidim.F"                /* number of output data */
*
*   [PARAM] 
      INTEGER    IDIMS, IDIME
      INTEGER    JDIMS, JDIME
*
*   [MODIFY] 
      REAL*8     GDATA ( IDIMS:IDIME, JDIMS:JDIME, * )
      INTEGER    IMAXD, JMAXD, KMAXD
*
*   [OUTPUT] 
      REAL*8     TOTLON, TOTLAT
*
*   [INPUT] 
      CHARACTER  HCORD *(*)
      REAL*8     RMISS
*
*   [INTERNAL WORK]
      REAL*8     DELX   ( NAXWRK )            !! work
      REAL*8     TLON, TLAT, TLEV, GD
      INTEGER    I, J, K
      INTEGER    IMAXE, JMAXE
*
      IMAXE = MIN( IMAXD,IDIME )
      JMAXE = MIN( JMAXD,JDIME )
      CALL HQCDXX( DELX(IDIMS), HCORD, IDIMS, IDIME-IDIMS+1 )
*
      DO 1100 K = 1, KMAXD
         DO 1100 J = JDIMS, JMAXE
            GD    = 0.
            TLON  = 0.
            DO 1110 I = IDIMS, IMAXE
               IF ( GDATA( I,J,K ) .NE. RMISS ) THEN
                  GD   = GD   + GDATA ( I,J,K ) * DELX( I )
                  TLON = TLON + DELX( I )
               ENDIF
 1110       CONTINUE
            IF ( TLON .GT. 0. ) THEN
               GDATA ( IDIMS,J,K ) = GD / TLON
            ELSE
               GDATA ( IDIMS,J,K ) = RMISS
            ENDIF
 1100 CONTINUE
*
      IMAXD  = IDIMS
      TOTLON = 0.D0
      DO 1500 I = IDIMS, IMAXE
         TOTLON = TOTLON + DELX( I )                  
 1500 CONTINUE
*
      RETURN
*=====================================================================
      ENTRY      YAVR      !! latitudinal mean
     M         ( GDATA , IMAXD , JMAXD , KMAXD ,
     O           TOTLAT,
     I           HCORD , RMISS ,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
      IMAXE = MIN( IMAXD,IDIME )
      JMAXE = MIN( JMAXD,JDIME )
      CALL HQCDXY( DELX(JDIMS), HCORD, JDIMS, JDIME-JDIMS+1 )
*
      DO 2100 K = 1, KMAXD
         DO 2100 I = IDIMS, IMAXE
            GD    = 0.
            TLAT  = 0.
            DO 2110 J = JDIMS, JMAXE
               IF ( GDATA( I,J,K ) .NE. RMISS ) THEN
                  GD   = GD   + GDATA ( I,J,K ) * DELX( J )
                  TLAT = TLAT + DELX( J )                  
               ENDIF
 2110       CONTINUE
            IF ( TLAT .GT. 0. ) THEN
               GDATA ( I,JDIMS,K ) = GD / TLAT
            ELSE
               GDATA ( I,JDIMS,K ) = RMISS
            ENDIF
 2100 CONTINUE
*
      JMAXD  = JDIMS
      TOTLAT = 0.D0
      DO 2500 J = JDIMS, JMAXE
         TOTLAT = TOTLAT + DELX( J )                  
 2500 CONTINUE
*
      RETURN
*======================================================================
      ENTRY      ZAVR      !! vertical average
     M         ( GDATA , IMAXD , JMAXD , KMAXD ,
     I           HCORD , RMISS ,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*     
      IMAXE = MIN( IMAXD,IDIME )
      JMAXE = MIN( JMAXD,JDIME )
      CALL HQCDXZ( DELX, HCORD, 1, KMAXD )
*
      DO 3100 J = JDIMS, JMAXE
         DO 3100 I = IDIMS, IMAXE
            GD   = 0.
            TLEV = 0.
            DO 3110 K = 1, KMAXD
               IF ( GDATA( I,J,K ) .NE. RMISS ) THEN
                  GD   = GD   + GDATA ( I,J,K ) * DELX( K )
                  TLEV = TLEV + DELX( K )
               ENDIF
 3110       CONTINUE
            IF ( TLEV .GT. 0. ) THEN
               GDATA ( I,J,1 ) = GD / TLEV
            ELSE
               GDATA ( I,J,1 ) = RMISS
            ENDIF
 3100 CONTINUE
*
      KMAXD = 1
*
      RETURN
      END
************************************************************************
      SUBROUTINE XSEL      !! select longitude
     M         ( GDATA , IMAXD , JMAXD , KMAXD ,
     I           ISEL  , RMISS ,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
*   [PARAM] 
      INTEGER    IDIMS, IDIME
      INTEGER    JDIMS, JDIME
*
*   [MODIFY] 
      REAL*8     GDATA ( IDIMS:IDIME, JDIMS:JDIME, * )
      INTEGER    IMAXD, JMAXD, KMAXD
*
*   [INPUT] 
      INTEGER    ISEL
      REAL*8     RMISS
*
*   [ENTRY INPUT] 
      INTEGER    KEXP
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K
      INTEGER    IMAXE, JMAXE
*
      IMAXE = MIN( IMAXD,IDIME )
      JMAXE = MIN( JMAXD,JDIME )
*
      IF ( ISEL .GT. IDIMS .AND. ISEL .LE. IMAXE ) THEN
         DO 1100 K = 1, KMAXD
            DO 1100 J = JDIMS, JMAXE
               GDATA ( IDIMS,J,K ) = GDATA ( ISEL,J,K )
 1100    CONTINUE
      ELSE IF ( ISEL .NE. IDIMS ) THEN
         DO 1200 K = 1, KMAXD
            DO 1200 J = JDIMS, JMAXE
               GDATA ( IDIMS,J,K ) = RMISS
 1200    CONTINUE
      ENDIF
*
      IMAXD = IDIMS
*
      RETURN
*================================================================
      ENTRY       YSEL      !! select latitude
     M         ( GDATA , IMAXD , JMAXD , KMAXD ,
     I           ISEL  , RMISS ,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
      IMAXE = MIN( IMAXD,IDIME )
      JMAXE = MIN( JMAXD,JDIME )
*
      IF ( ISEL .GT. JDIMS .AND. ISEL .LE. JMAXE ) THEN
         DO 2100 K = 1, KMAXD
            DO 2100 I = IDIMS, IMAXE
               GDATA ( I,JDIMS,K ) = GDATA ( I,ISEL,K )
 2100    CONTINUE
      ELSE IF ( ISEL .NE. JDIMS ) THEN
         DO 2200 K = 1, KMAXD
            DO 2200 I = IDIMS, IMAXE
               GDATA ( I,JDIMS,K ) = RMISS
 2200    CONTINUE
      ENDIF
*
      JMAXD = JDIMS
*
      RETURN
*================================================================
      ENTRY      ZSEL      !! select vertical level
     M         ( GDATA , IMAXD , JMAXD , KMAXD ,
     I           ISEL  , RMISS ,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
      IMAXE = MIN( IMAXD,IDIME )
      JMAXE = MIN( JMAXD,JDIME )
*
      IF ( ISEL .GT. 1 .AND. ISEL .LE. KMAXD ) THEN
         DO 3100 J = JDIMS, JMAXE
            DO 3100 I = IDIMS, IMAXE
               GDATA ( I,J,1 ) = GDATA ( I,J,ISEL )
 3100    CONTINUE
      ELSE IF ( ISEL .NE. 1 ) THEN
         DO 3200 J = JDIMS, JMAXE
            DO 3200 I = IDIMS, IMAXE
               GDATA ( I,J,1 ) = RMISS
 3200    CONTINUE
      ENDIF
*
      KMAXD = 1
*
      RETURN
*================================================================
      ENTRY      ZEXT      !! vertical extension
     M         ( GDATA , IMAXD , JMAXD , KMAXD ,
     I           KEXP  ,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
      IMAXE = MIN( IMAXD,IDIME )
      JMAXE = MIN( JMAXD,JDIME )
*
      DO 4100 K = KMAXD+1, KEXP
         DO 4100 J = JDIMS, JMAXE
            DO 4100 I = IDIMS, IMAXE
               GDATA ( I,J,K ) = GDATA ( I,J,KMAXD )
 4100 CONTINUE
*
      KMAXD = KEXP
*
      RETURN
      END
************************************************************************
      SUBROUTINE DIURAV    !! averaged diurnal cycle output
     M         ( GDATA , IMAXD , JMAXD , KMAXD ,
     I           IDIUR , IXSEL , HCORD , RMISS ,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zidim.F"                /* number of output data */
*
*   [PARAM] 
      INTEGER    IDIMS, IDIME
      INTEGER    JDIMS, JDIME
*
*   [MODIFY] 
      REAL*8     GDATA ( IDIMS:IDIME, JDIMS:JDIME, * )
      INTEGER    IMAXD, JMAXD, KMAXD
*
*   [INPUT] 
      INTEGER    IDIUR
      INTEGER    IXSEL
      CHARACTER  HCORD *(*)
      REAL*8     RMISS
*
*   [INTERNAL WORK]
      REAL*8     ALON ( NAXWRK )            !! work
      REAL * 8   TIME
      REAL*8     RSEC
      INTEGER    I, J, K
      INTEGER    IDAYS, ISEC, IHR, NSECDY, NSECHR
      INTEGER    IMAXE, JMAXE
      REAL*8     PI
*
      IMAXE = MIN( IMAXD,IDIME )
      JMAXE = MIN( JMAXD,JDIME )
      PI    = ATAN(1.D0)*4.D0
*      
      CALL GETTIM( TIME )
      CALL CSS2DS( IDAYS, RSEC, TIME )
      CALL CSECDY( NSECDY )
      CALL CSECHR( NSECHR )
*
      KMAXD = MAX(NSECDY/(NSECHR*ABS(IDIUR)),1)
      DO 1100 K = 2, KMAXD
         DO 1110 J = JDIMS, JMAXE         
            DO 1110 I = IDIMS, IMAXE
               GDATA( I,J,K ) = RMISS
 1110    CONTINUE 
 1100 CONTINUE 
*
      CALL HQCAXX( ALON(IDIMS), HCORD, IDIMS, IDIME-IDIMS+1 )
*
      DO 2100 I = IDIMS, IMAXE
         IF ( IXSEL .GT. 0 ) THEN
            ISEC = NINT( RSEC + NSECDY*ALON( IXSEL )/(2.*PI) )
         ELSE
            ISEC = NINT( RSEC + NSECDY*ALON( I )/(2.*PI) )
         ENDIF
         IHR  = NINT( MOD( ISEC,NSECDY )/REAL(NSECHR) )
         IF ( IDIUR .GT. 0 .OR. MOD(IHR,ABS(IDIUR)) .EQ. 0 ) THEN
            K    = NINT(REAL(IHR)/ABS(IDIUR))+1
            IF ( K .GT. KMAXD ) K = K - KMAXD
            IF ( K .NE. 1 ) THEN
               DO 2110 J = JDIMS, JMAXE
                  GDATA( I,J,K ) = GDATA( I,J,1 )
                  GDATA( I,J,1 ) = RMISS
 2110          CONTINUE 
            ENDIF
         ELSE
            DO 2200 J = JDIMS, JMAXE
               GDATA( I,J,1 ) = RMISS
 2200       CONTINUE 
         ENDIF
 2100 CONTINUE 
*
      RETURN
*====================================================================
      ENTRY      DIURSL            !! diurnal variation
     M         ( GDATA , IMAXD , JMAXD , KMAXD ,
     I           IDIUR , HCORD , 
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
      IMAXE = MIN( IMAXD,IDIME )
      JMAXE = MIN( JMAXD,JDIME )
      PI    = ATAN(1.D0)*4.D0
*      
      CALL GETTIM( TIME )
      CALL CSS2DS( IDAYS, RSEC, TIME )
      CALL CSECDY( NSECDY )
      CALL CSECHR( NSECHR )
*
      CALL HQCAXX( ALON(IDIMS), HCORD, IDIMS, IDIME-IDIMS+1 )
*
      DO 3100 I = IDIMS, IMAXE
         ISEC = NINT( RSEC + NSECDY*ALON( I )/(2.*PI) )
         IHR  = NINT( MOD( ISEC,NSECDY )/REAL(NSECHR) )
         K    = NINT(REAL(IHR)/ABS(IDIUR))+1
         IF ( K .GT. KMAXD ) K = K - KMAXD
         IF ( K .NE. 1 ) THEN
            DO 3110 J = JDIMS, JMAXE
               GDATA( I,J,1 ) = GDATA( I,J,K )
 3110       CONTINUE 
         ENDIF
 3100 CONTINUE 
*
      KMAXD = 1
*
      RETURN
      END
