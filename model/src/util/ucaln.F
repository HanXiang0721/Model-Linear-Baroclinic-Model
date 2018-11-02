* PACKAGE UCALN !! admin: control date
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
********************************************************************
      SUBROUTINE UCALN     !! calendar
*
*       epoch = 0000/01/00
*       (...don't care about negative value...)
*
*   [ENTRY INPUT]
      REAL * 8   DSEC                 !! time
      INTEGER    NDAYMO               !! No.of day in a month
      INTEGER    NDAYYR               !! No.of day in an year
      INTEGER    NMONYR               !! No.of month in an year
      INTEGER    IDAYS                !! serial No.of day
      INTEGER    IYEAR                !! year
      INTEGER    IMONTH               !! month
      INTEGER    IDAY                 !! day
      INTEGER    NSECDY               !! No. of sec. in a day
      INTEGER    NSECHR               !! No.of sec. in an hour
      INTEGER    NSECMI               !! No. of sec. in a minute
      INTEGER    IHOUR                !! hour
      INTEGER    IMIN                 !! minute
      INTEGER    ISEC                 !! second
      INTEGER    IDAYSY               !! ordinal # of day in an year
      REAL*8     RSEC                 !! No. of sec. in a day
      LOGICAL    OOPERP               !! output of perpetual mode ?
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
      INTEGER    ISECDY, ISECHR, JY, JY4, JCENT, JCENT4
      INTEGER    IDAYS0, IDY, ILEAP, ID, M, IDAYYR, JYEAR, JMONTH
*
*   [EXTERNAL FUNC] 
      LOGICAL    OCLEAP
*
*   [INTERNAL PARM] 
      LOGICAL    OAUTO               !! YR=0-999     : 360day
                                     !! YR=1000-1899 : 365day
                                     !! YR=1900-     : Gregorian
      LOGICAL    OGREGO              !! the Gregorian calendar ?
      INTEGER    MONDAY ( 12,2 )     !! 1month = Xdays(j=2:leap)
      LOGICAL    OIDEAL              !! ideal calender (n day per month)
      INTEGER    IDAYMO              !! 1 month = X days
      INTEGER    IMONYR              !! 1 year = X months
      LOGICAL    OPERPT              !! perpetual ?
      INTEGER    IYRPP               !! perpetual date(year)
      INTEGER    IMONPP              !! perpetual date(month)
      INTEGER    IDAYPP              !! perpetual date(day)
      INTEGER    ISECMN              !! 1 minute = X sec.
      INTEGER    IMINHR              !! 1 hour = X minutes
      INTEGER    IHRDAY              !! 1 day = X hours
      LOGICAL    OOPERZ
      LOGICAL    OFIRST 
      NAMELIST  /NMCALN/ 
     &           OAUTO , OGREGO, OIDEAL, OPERPT,
     &           IDAYMO, IMONYR, IYRPP, IMONPP, IDAYPP, 
     &           ISECMN, IMINHR, IHRDAY
      DATA       OAUTO  / .TRUE. /
      DATA       OGREGO / .TRUE. /
      DATA       MONDAY / 31,28,31,30,31,30,31,31,30,31,30,31,
     &                    31,29,31,30,31,30,31,31,30,31,30,31 /
*C                         1  2  3  4  5  6  7  8  9 10 11 12
      DATA       OIDEAL / .FALSE. /
      DATA       IDAYMO, IMONYR / 30, 12 / 
      DATA       OPERPT / .FALSE. /
      DATA       IYRPP , IMONPP, IDAYPP / 0, 3, 21 /
      DATA       ISECMN, IMINHR, IHRDAY / 60, 60, 24 /
      DATA       OOPERZ / .TRUE. /
      DATA       OFIRST / .TRUE. /
*
*==================================================================
      ENTRY      CALNDR    !! initialize calendar
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ ACALN: CALENDAR 93/11/12'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMCALN, END=190 )
  190    WRITE  ( JFPAR, NMCALN )
      ENDIF
*
      RETURN
*==================================================================
      ENTRY      CPERPT    !! calendar, fixed date
     I         ( IYEAR , IMONTH, IDAY  )
*
      OPERPT = .TRUE.
      IYRPP  = IYEAR
      IMONPP = IMONTH
      IDAYPP = IDAY
*
      RETURN
*==================================================================
      ENTRY      CPERPO    !! calendar, fixed date(on/off)
     I         ( OOPERP )
*
      OOPERZ = OOPERP
*
      RETURN
*==================================================================
      ENTRY      CDAYMO    !! calendar, No.of day in a month
     O         ( NDAYMO,
     I           IYEAR , IMONTH  )
*
      IF ( OAUTO  ) THEN
         IF ( IYEAR .GE. 1900 ) THEN
            OGREGO = .TRUE.
         ELSE
            OGREGO = .FALSE.
            IF ( IYEAR .GE. 1000 ) THEN
               OIDEAL = .FALSE.
            ELSE
               OIDEAL = .TRUE.
               IDAYMO = 30
               IMONYR = 12
            ENDIF
         ENDIF
      ENDIF
*
      IF ( OGREGO ) THEN
         IF ( OCLEAP( IYEAR ) ) THEN
            NDAYMO = MONDAY( IMONTH,2 )
         ELSE
            NDAYMO = MONDAY( IMONTH,1 )            
         ENDIF
      ELSE IF ( .NOT. OIDEAL ) THEN
         NDAYMO = MONDAY( IMONTH,1 )            
      ELSE
         NDAYMO = IDAYMO
      ENDIF
*
      RETURN
*==================================================================
      ENTRY      CDAYYR    !! calendar, No.of day in an year
     O         ( NDAYYR,
     I           IYEAR  )
*
      IF ( OAUTO  ) THEN
         IF ( IYEAR .GE. 1900 ) THEN
            OGREGO = .TRUE.
         ELSE
            OGREGO = .FALSE.
            IF ( IYEAR .GE. 1000 ) THEN
               OIDEAL = .FALSE.
            ELSE
               OIDEAL = .TRUE.
               IDAYMO = 30
               IMONYR = 12
            ENDIF
         ENDIF
      ENDIF
*
      IF ( OGREGO ) THEN
         IF ( OCLEAP( IYEAR ) ) THEN
            NDAYYR = 366
         ELSE
            NDAYYR = 365
         ENDIF
      ELSE IF ( .NOT. OIDEAL ) THEN
         NDAYYR = 365
      ELSE
         NDAYYR = IDAYMO*IMONYR
      ENDIF
*
      RETURN
*==================================================================
      ENTRY      CMONYR    !! calendar, No.of month in an year
     O         ( NMONYR, 
     I           IYEAR   )
*
      IF ( OAUTO  ) THEN
         NMONYR = 12
      ELSE
         NMONYR = IMONYR
      ENDIF
*
      RETURN
*==================================================================
      ENTRY      CSECDY    !! calendar, No.of sec. in a day
     O         ( NSECDY )
*
      NSECDY = ISECMN*IMINHR*IHRDAY
*
      RETURN
*==================================================================
      ENTRY      CSECMI    !! calendar, No of sec. in a minute
     O         ( NSECMI )
*
      NSECMI = ISECMN
*
      RETURN
*==================================================================
      ENTRY      CSECHR    !! calendar, No.of sec. in an hour
     O         ( NSECHR )
*
      NSECHR = ISECMN*IMINHR
*
      RETURN
*==================================================================
      ENTRY      CSS2DS    !! calendar, sec. -> ddss
     O         ( IDAYS , RSEC  ,
     I           DSEC            )
*
      ISECDY = ISECMN*IMINHR*IHRDAY
      IDAYS  = INT( DSEC/DBLE (ISECDY) ) + 1
      RSEC   = DSEC - DBLE (IDAYS-1)*DBLE (ISECDY)
      IF ( NINT( RSEC ) .GE. ISECDY ) THEN
         IDAYS = IDAYS + 1
         RSEC  = RSEC - DBLE(ISECDY)
      ENDIF
*
      RETURN
*==================================================================
      ENTRY      CDS2SS    !! calendar, ddss -> sec.
     I         ( DSEC  ,
     O           IDAYS , RSEC   )
*
      ISECDY = ISECMN*IMINHR*IHRDAY
      DSEC   = DBLE (IDAYS-1)*DBLE (ISECDY) + DBLE (RSEC)
*
      RETURN
*==================================================================
      ENTRY      CRS2HM    !! calendar, sec. -> hhmmss
     O         ( IHOUR , IMIN  , ISEC  ,
     I           RSEC                   )
*
      ISECHR = ISECMN*IMINHR
      IHOUR  = INT ( RSEC / DBLE(ISECHR ) )
      IMIN   = INT ( ( RSEC - DBLE(IHOUR*ISECHR) )/DBLE(ISECMN) )
      ISEC   = NINT( RSEC - DBLE(IHOUR*ISECHR) - DBLE(IMIN*ISECMN) )
      IF ( ISEC .GE. ISECMN ) THEN
         IMIN  = IMIN + 1
         ISEC  = ISEC - ISECMN
      ENDIF
      IF ( IMIN .EQ. IMINHR ) THEN
         IHOUR = IHOUR + 1
         IMIN  = IMIN  - IMINHR
      ENDIF
*
      RETURN
*==================================================================
      ENTRY      CHM2RS    !! calendar, hhmmss -> sec.
     O         ( RSEC  ,
     I           IHOUR , IMIN  , ISEC   )
*
      RSEC = IHOUR*ISECMN*IMINHR + IMIN*ISECMN + ISEC
*
      RETURN
*==================================================================
      ENTRY      CDD2YM    !! calendar, day -> yymmdd
     O         ( IYEAR , IMONTH, IDAY  ,
     I           IDAYS                   )
*
      IF ( OAUTO  ) THEN
         IF ( IDAYS .GE. 693961 ) THEN       !! 1900*365+1900/4-19+5
            OGREGO = .TRUE.
         ELSE
            OGREGO = .FALSE.
            IF ( IDAYS .GE. 1000*365 ) THEN
               OIDEAL = .FALSE.
            ELSE
               OIDEAL = .TRUE.
               IDAYMO = 30
               IMONYR = 12
            ENDIF
         ENDIF
      ENDIF
*
      IF ( OPERPT .AND. OOPERZ ) THEN
         IYEAR  = IYRPP
         IMONTH = IMONPP
         IDAY   = IDAYPP
*
         RETURN
*
      ENDIF
*
      IF ( OGREGO ) THEN
         JY     = INT(DBLE(IDAYS)/365.24)
 1100    CONTINUE 
            JY4    = (JY+3)/4
            JCENT  = (JY+99)/100
            JCENT4 = (JY+399)/400
            IDAYS0 = JY*365+JY4-JCENT+JCENT4
            IF ( IDAYS .LE. IDAYS0 ) THEN
               JY = JY -1 
               IF ( JY .GE. 0 ) GOTO 1100
            ENDIF
         IYEAR = JY
         IDY   = IDAYS - IDAYS0
         IF ( OCLEAP( IYEAR ) ) THEN
            ILEAP  = 2
         ELSE
            ILEAP  = 1
         ENDIF
      ELSE IF ( .NOT. OIDEAL ) THEN
         IYEAR = IDAYS/365
         IDY   = IDAYS - IYEAR*365
         ILEAP = 1
      ENDIF
*
      IF ( OGREGO .OR. .NOT. OIDEAL ) THEN
         ID = 0
         DO 3100 M = 1, 12
            ID = ID + MONDAY(M,ILEAP)
            IF ( IDY .LE. ID ) THEN
               IMONTH = M
               IDAY   = IDY-ID+MONDAY(M,ILEAP)
               GOTO 3190
            ENDIF
 3100    CONTINUE 
 3190    CONTINUE 
      ELSE 
         IDAYYR = IDAYMO*IMONYR
         IYEAR  = ( IDAYS-1 ) / IDAYYR
         IMONTH = ( IDAYS-1 - IYEAR*IDAYYR )/IDAYMO+1
         IDAY   = IDAYS - IYEAR*IDAYYR - (IMONTH-1)*IDAYMO
      ENDIF
*
      RETURN
*======================================================================
      ENTRY      CYM2DD    !! calendar, yymmdd -> day
     O         ( IDAYS ,
     I           IYEAR , IMONTH, IDAY   )
*
      IF ( OAUTO  ) THEN
         IF ( IYEAR .GE. 1900 ) THEN
            OGREGO = .TRUE.
         ELSE
            OGREGO = .FALSE.
            IF ( IYEAR .GE. 1000 ) THEN
               OIDEAL = .FALSE.
            ELSE
               OIDEAL = .TRUE.
               IDAYMO = 30
               IMONYR = 12
            ENDIF
         ENDIF
      ENDIF
*
      IF ( OGREGO .OR. .NOT. OIDEAL ) THEN
         IF ( IMONTH .GT. 0 ) THEN
            JYEAR  = IYEAR + (IMONTH-1)/12
            JMONTH = MOD(IMONTH-1,12)+1
         ELSE
            JYEAR  = IYEAR - (-IMONTH)/12 - 1
            JMONTH = 12-MOD(-IMONTH,12)
         ENDIF
      ENDIF

      IF ( OGREGO ) THEN
         JY4    = (JYEAR+3)/4
         JCENT  = (JYEAR+99)/100
         JCENT4 = (JYEAR+399)/400
         IDAYS0 = JYEAR*365+JY4-JCENT+JCENT4
         IF ( OCLEAP( JYEAR ) ) THEN
            ILEAP = 2
         ELSE
            ILEAP = 1
         ENDIF
      ELSE IF ( .NOT. OIDEAL ) THEN
         IDAYS0 = JYEAR*365
         ILEAP  = 1
      ENDIF

      IF ( OGREGO .OR. .NOT. OIDEAL ) THEN
         ID = 0
         DO 4100 M = 1, JMONTH-1
            ID = ID + MONDAY(M,ILEAP)
 4100    CONTINUE 
      ELSE
         IDAYS0 = IYEAR*IDAYMO*IMONYR
         ID     = (IMONTH-1)*IDAYMO
      ENDIF
*
      IDAYS = IDAYS0 + ID + IDAY
*      
      RETURN
*======================================================================
      ENTRY      CYM2YD    !! calendar, yymmdd -> yydd
     O         ( IDAYSY,
     I           IYEAR , IMONTH, IDAY   )
*
      IF ( OAUTO  ) THEN
         IF ( IYEAR .GE. 1900 ) THEN
            OGREGO = .TRUE.
         ELSE
            OGREGO = .FALSE.
            IF ( IYEAR .GE. 1000 ) THEN
               OIDEAL = .FALSE.
            ELSE
               OIDEAL = .TRUE.
               IDAYMO = 30
               IMONYR = 12
            ENDIF
         ENDIF
      ENDIF
*
      IF ( OGREGO .OR. .NOT. OIDEAL ) THEN
         IF ( IMONTH .GT. 0 ) THEN
            JYEAR  = IYEAR + (IMONTH-1)/12
            JMONTH = MOD(IMONTH-1,12)+1
         ELSE
            JYEAR  = IYEAR - (-IMONTH)/12 - 1
            JMONTH = 12-MOD(-IMONTH,12)
         ENDIF
      ENDIF

      IF ( OGREGO ) THEN
         IF ( OCLEAP( JYEAR ) ) THEN
            ILEAP = 2
         ELSE
            ILEAP = 1
         ENDIF
      ELSE IF ( .NOT. OIDEAL ) THEN
         ILEAP  = 1
      ENDIF

      IF ( OGREGO .OR. .NOT. OIDEAL ) THEN
         ID = 0
         DO 5100 M = 1, JMONTH-1
            ID = ID + MONDAY(M,ILEAP)
 5100    CONTINUE 
      ELSE
         ID     = (IMONTH-1)*IDAYMO
      ENDIF
*
      IDAYSY = ID + IDAY
*      
      RETURN
      END
*********************************************************************
      SUBROUTINE CSS2YH    !! calendar, sec. -> date
     O         ( IDATE ,
     I           DSEC   )
*
*   [OUTPUT] 
      INTEGER    IDATE ( 6 )       !! yymmddhhmmss
*   [INPUT] 
      REAL * 8   DSEC              !! time
*   [INTERNAL WORK] 
      INTEGER    IDAYS             !! serial No.of day
      REAL*8     RSEC              !! No. of sec. in a day
*
      CALL       CSS2DS
     O         ( IDAYS , RSEC  ,
     I           DSEC            )
      CALL       CDD2YM
     O         ( IDATE(1), IDATE(2), IDATE(3),
     I           IDAYS                        )
      CALL       CRS2HM
     O         ( IDATE(4), IDATE(5), IDATE(6),
     I           RSEC                          )
*
      RETURN
*===============================================================
      ENTRY      CYH2SS    !! calendar, date -> sec.
     O         ( DSEC  ,
     I           IDATE  )
*
      CALL       CYM2DD
     O         ( IDAYS   ,
     I           IDATE(1), IDATE(2), IDATE(3) )
      CALL       CHM2RS
     O         ( RSEC    ,
     I           IDATE(4), IDATE(5), IDATE(6) )
      CALL       CDS2SS
     O         ( DSEC  ,
     I           IDAYS , RSEC   )
*
      RETURN
      END
*********************************************************************
      SUBROUTINE CDD2YD    !! calendar, day -> yydd
     O         ( IYEAR , IDAYSY,
     I           IDAYS           )
*
*   [OUTPUT]       
      INTEGER    IYEAR
      INTEGER    IDAYSY
*   [INPUT] 
      INTEGER    IDAYS
*   [INTERNAL WORK] 
      INTEGER    IMONTH, IDAY
*
      CALL CDD2YM
     O         ( IYEAR , IMONTH, IDAY  ,
     I           IDAYS                   )
      CALL CYM2YD
     O         ( IDAYSY,
     I           IYEAR , IMONTH, IDAY    )
*
      RETURN
      END
********************************************************************
      SUBROUTINE CSS2YD    !! calendar, sec. -> yydd
     O         ( IYEAR , IDAYSY,
     I           DSEC           )
*
*   [OUTPUT]       
      INTEGER    IYEAR
      INTEGER    IDAYSY
*   [INPUT] 
      REAL * 8   DSEC
*   [INTERNAL WORK] 
      INTEGER    IMONTH, IDAY
*
      CALL CSS2YM
     O         ( IYEAR , IMONTH, IDAY  ,
     I           DSEC                    )
      CALL CYM2YD
     O         ( IDAYSY,
     I           IYEAR , IMONTH, IDAY    )
*
      RETURN
      END
*********************************************************************
      SUBROUTINE CSS2YM    !! calendar, sec. -> yymmdd
     O         ( IYEAR , IMONTH, IDAY  ,
     I           DSEC                    )
*
*   [OUTPUT] 
      INTEGER    IYEAR
      INTEGER    IMONTH
      INTEGER    IDAY
*   [INPUT] 
      REAL * 8   DSEC
*   [INTERNAL WORK] 
      INTEGER    IDAYS
      REAL*8     RSEC
*
      CALL       CSS2DS
     O         ( IDAYS , RSEC  ,
     I           DSEC            )
      CALL       CDD2YM
     O         ( IYEAR , IMONTH, IDAY  ,
     I           IDAYS                  )
*
      RETURN
      END
*********************************************************************
      SUBROUTINE CXX2SS    !! calendar, hour ->sec.
     O         ( DDSEC ,
     I           RTDUR , HUNIT, DSEC  )
*
      REAL * 8   DDSEC
      REAL*8     RTDUR
      CHARACTER  HUNIT *(*)
      REAL * 8   DSEC
*
      CHARACTER  HUNITX *10
      INTEGER    ISECMI, ISECHR, ISECDY
      INTEGER    IYEAR, IMONTH, IDAY, NDAYMO, NDAYYR
*
      HUNITX = HUNIT
C     CALL CUPPER( HUNITX )
*
      IF      ( HUNITX(1:1) .EQ. 'S' ) THEN
         DDSEC = DBLE (RTDUR)
      ELSE IF ( HUNITX(1:2) .EQ. 'MI' ) THEN
         CALL  CSECMI( ISECMI )
         DDSEC = DBLE (RTDUR)*DBLE (ISECMI)
      ELSE IF ( HUNITX(1:1) .EQ. 'H' ) THEN
         CALL  CSECHR( ISECHR )         
         DDSEC = DBLE (RTDUR)*DBLE (ISECHR)
      ELSE IF ( HUNITX(1:1) .EQ. 'D' ) THEN      
         CALL  CSECDY( ISECDY )         
         DDSEC = DBLE (RTDUR)*DBLE (ISECDY)
      ELSE IF ( HUNITX(1:2) .EQ. 'MO' ) THEN
         CALL  CSS2YM
     O         ( IYEAR , IMONTH, IDAY  ,
     I           DSEC                    )
         CALL  CDAYMO
     O         ( NDAYMO,
     I           IYEAR , IMONTH  )
         CALL  CSECDY( ISECDY )         
         DDSEC = DBLE (RTDUR)*DBLE (NDAYMO)*DBLE (ISECDY)
      ELSE IF ( HUNITX(1:1) .EQ. 'Y' ) THEN
         CALL  CSS2YM
     O         ( IYEAR , IMONTH, IDAY  ,
     I           DSEC                    )
         CALL  CDAYYR
     O         ( NDAYYR,
     I           IYEAR   )
         CALL  CSECDY( ISECDY )         
         DDSEC = DBLE (RTDUR)*DBLE (NDAYYR)*DBLE (ISECDY)
      ELSE
         WRITE (6,*) ' ### CXX2SS: INVALID UNIT : ', HUNIT,
     &               ' [SEC] ASSUMED'
         DDSEC = RTDUR
      ENDIF
*
      RETURN
      END
*********************************************************************
      SUBROUTINE CCC2YH    !! calendar, character -> date
     O         ( ITIME ,
     I           HTIME  )
*
*   [OUTPUT] 
      INTEGER    ITIME ( 6 )
*   [INPUT] 
      CHARACTER  HTIME *(*)
*   [INTERNAL WORK] 
      INTEGER    I
*
      READ ( HTIME, 2600 ) (ITIME(I),I=1,6)
 2600 FORMAT( I4.4,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2 )
*
      RETURN
      END
*********************************************************************
      SUBROUTINE CYH2CC    !! calendar, date -> character
     O         ( HTIME ,
     I           ITIME  )
*
*   [OUTPUT] 
      CHARACTER  HTIME *(*)
*   [INPUT] 
      INTEGER    ITIME ( 6 )
*   [INTERNAL WORK] 
      INTEGER    I
*
      WRITE ( HTIME, 600 ) (ITIME(I),I=1,6)
  600 FORMAT( I4.4,'/',I2.2,'/',I2.2,'-',I2.2,':',I2.2,':',I2.2 )
*
      RETURN
      END
*********************************************************************
      SUBROUTINE CSS2CC    !! calendar, sec. -> character (NO PERPET.)
     O         ( HTIME ,
     I           DSEC    )
*
*   [OUTPUT] 
      CHARACTER  HTIME *(*)
*   [INPUT] 
      REAL*8     DSEC
*   [INTERNAL WORK] 
      INTEGER    ITIME(6)
*
      CALL CPERPO ( .FALSE. )
      CALL CSS2YH
     O         ( ITIME ,
     I           DSEC   )
      CALL CYH2CC
     O         ( HTIME ,
     I           ITIME  )
      CALL CPERPO ( .TRUE.  )
*
      RETURN
      END
*********************************************************************
      FUNCTION   OCLEAP    !! calendar : leap year or not
     I         ( IYEAR )
*
      LOGICAL    OCLEAP
*   [INPUT] 
      INTEGER    IYEAR
*
*   [INTERNAL WORK] 
      INTEGER    IY, IYCEN, ICENT
*
      IY     = MOD(IYEAR,4)
      IYCEN  = MOD(IYEAR,100)
      ICENT  = MOD(IYEAR/100,4)
*
      IF ( IY .EQ. 0 .AND. ( IYCEN .NE. 0 .OR. ICENT .EQ. 0 ) ) THEN
         OCLEAP = .TRUE.
      ELSE
         OCLEAP = .FALSE.
      ENDIF
*
      RETURN
      END
*********************************************************************
      SUBROUTINE CSSAFT    !! calendar, time advancing
     O         ( DSECA ,
     I           DSEC  , RAFTR , HUNIT  )
*
*   [OUTPUT] 
      REAL * 8   DSECA
*   [INPUT] 
      REAL * 8   DSEC
      REAL*8     RAFTR
      CHARACTER  HUNIT *(*)
*   [INTERNAL WORK] 
      INTEGER    IDAYS, IYEAR, IMONTH, IDAY
      REAL*8     RSEC
      REAL * 8   DDTIME
*
      IF ( HUNIT(1:1) .EQ. 'Y' .OR. HUNIT(1:2) .EQ. 'MO' ) THEN
         CALL    CSS2DS
     O         ( IDAYS , RSEC  ,
     I           DSEC            )
         CALL    CDD2YM
     O         ( IYEAR , IMONTH, IDAY  ,
     I           IDAYS                  )
         IF      ( HUNIT(1:1) .EQ. 'Y' ) THEN
            IYEAR  = IYEAR  + INT(RAFTR)
         ELSE IF ( HUNIT(1:2) .EQ. 'MO' ) THEN
            IMONTH = IMONTH + INT(RAFTR)
         ENDIF
         CALL    CYM2DD
     O         ( IDAYS ,
     I           IYEAR , IMONTH, IDAY   )
         CALL    CDS2SS
     O         ( DSECA ,
     I           IDAYS , RSEC    )
      ELSE
         CALL CXX2SS
     O         ( DDTIME,
     I           RAFTR , HUNIT , DSEC  )
         DSECA = DSEC + DDTIME
      ENDIF
*
      RETURN
      END
************************************************************************
      FUNCTION   OINTVL    !! time step passed ?
     I         ( DTIME , DTPREV , DTORGN, RINTV , HTUNIT )
*
      LOGICAL    OINTVL
*   [INPUT] 
      REAL * 8   DTIME 
      REAL * 8   DTPREV 
      REAL * 8   DTORGN       
      REAL*8     RINTV
      CHARACTER  HTUNIT *(*)
*
*   [INTERNAL WORK] 
      REAL * 8   DDTIME
      CHARACTER  HUNIT *5
      INTEGER    IYEAR, IMON, IDAY, IYEARP, IMONP, IDAYP
      INTEGER    IY, IMO
      INTEGER    NMONYR, NDAYYR, NDAYMO
      REAL*8     RY, RMO
*
*   [EXTERNAL FUNC] 
      REAL * 8   DGAUS
*
      HUNIT = HTUNIT
C     CALL CUPPER( HUNIT )
*
      IF ( DTIME .EQ. DTPREV ) THEN
         OINTVL = .TRUE.         
         RETURN
      ENDIF
*
      OINTVL = .FALSE.
      CALL CSS2YM
     O         ( IYEAR , IMON  , IDAY  ,
     I           DTIME                   )
      CALL CSS2YM
     O         ( IYEARP, IMONP , IDAYP ,
     I           DTPREV                  )
      CALL CXX2SS
     O         ( DDTIME,
     I           RINTV , HUNIT , DTIME )
*
      IF ( DTIME .GE. DTORGN ) THEN
         IF      ( HUNIT(1:1) .EQ. 'Y' ) THEN
            CALL CMONYR( NMONYR, IYEAR )
            CALL CDAYYR( NDAYYR, IYEAR )
            RY = IYEAR - IYEARP
     &         + DBLE( IMON-IMONP )/DBLE( NMONYR )
     &         + DBLE( IDAY-IDAYP )/DBLE( NDAYYR )
            IF ( RY .GE. RINTV ) THEN
               OINTVL = .TRUE.
            ENDIF
         ELSE IF ( HUNIT(1:2) .EQ. 'MO' ) THEN
            IMO = 0
            DO 2100 IY = IYEARP, IYEAR-1
               CALL CMONYR( NMONYR, IY )
               IMO = IMO + NMONYR
 2100       CONTINUE 
            CALL CDAYMO( NDAYMO, IYEAR, IMON )
            RMO = IMON - IMONP + IMO 
     &          + DBLE( IDAY-IDAYP )/DBLE( NDAYMO )
            IF ( RMO .GE. RINTV ) THEN
               OINTVL = .TRUE.
            ENDIF
         ELSE IF (      DGAUS((DTIME -DTORGN)/DDTIME) 
     &             .GT. DGAUS((DTPREV-DTORGN)/DDTIME) ) THEN
               OINTVL = .TRUE.
         ENDIF
      ENDIF
*
      RETURN
      END
