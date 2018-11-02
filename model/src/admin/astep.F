* PACKAGE ASTEP !! admin: control time step
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/06(numaguti) TORIGN
**********************************************************************
      SUBROUTINE TIMSTP    !!  control time step
     M         ( DELTT , OINIT ,
     I           TIME  , GDU   , GDV   , ISTEP  )
*
*   [MODIFY] 
      REAL*8     DELTT                       !! time step delta(t)
      LOGICAL    OINIT                       !! initial or not
*   [INPUT]       
      REAL * 8   TIME                        !! time
      REAL*8     GDU   ( * )                 !! zonal wind
      REAL*8     GDV   ( * )                 !! meridional wind
      INTEGER    ISTEP                       !! serial No. of step
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
      LOGICAL    OCALC
*
*   [EXTERNAL FUNC] 
      LOGICAL    OINTVL
*
*    [INTERNAL SAVE]     
      REAL * 8   DDELT
      REAL * 8   DDTMIN
      REAL * 8   DDTLCM
      REAL * 8   TPREV
      REAL * 8   TORIGN
      REAL*8     DELTX
      SAVE       DDELT, DDTMIN, DDTLCM, TPREV, TORIGN, DELTX
*
*   [INTERNAL PARM] 
      REAL*8     DELT         !! standard time step
      CHARACTER  TUNIT*4      !! unit
      INTEGER    INISTP       !! No.of initial time step
      REAL*8     DTMIN        !! minimum time step
      REAL*8     DTLCM        !! (minimum)common multiple
      REAL*8     SAFER        !! safety factor
      REAL*8     INTCAL       !! calculation interval
      LOGICAL    OFIRST
      NAMELIST   /NMDELT/ DELT, TUNIT , INISTP, DTMIN, DTLCM, SAFER,
     &                    INTCAL
      DATA       DELT   / 20.D0   /
      DATA       TUNIT  / 'MIN'   /
      DATA       INISTP / 3       /
      DATA       DTMIN  / 1.D0    /
      DATA       DTLCM  / 60.D0   /
      DATA       SAFER  / 0.8D0   /
      DATA       INTCAL / 120.D0  /
      DATA       OFIRST /  .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ ASTEP : TIME STEP ESTIMATION 93/11/17'
CCC      OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMDELT, END=190 )
  190    WRITE  ( JFPAR, NMDELT )
*
         CALL CXX2SS ( DDELT , DELT  , TUNIT, TIME )
         CALL CXX2SS ( DDTMIN, DTMIN , TUNIT, TIME )
         CALL CXX2SS ( DDTLCM, DTLCM , TUNIT, TIME )
         TPREV  = TIME
         TORIGN = 0.D0
         DELTX  = DDELT
      ENDIF
*
      IF ( INTCAL .GT. 0. ) THEN
         OCALC = OINTVL          !! time step passed ?
     I         ( TIME  , TPREV , TORIGN, INTCAL, TUNIT )
         IF ( OCALC .OR. OFIRST ) THEN
            CALL CALCDT
     O           ( DELTX ,
     I             DBLE(DDELT) , GDU   , GDV   , 
     I             DBLE(DDTMIN), DBLE(DDTLCM)  , SAFER   )
            TPREV = TIME
         ENDIF
      ENDIF
*
      DELTT  = DELTX
      IF ( ISTEP .GT. INISTP ) THEN
         OINIT  = .FALSE.
      ELSE IF ( OINIT ) THEN
         DELTT  = DELTX  / ( 2.D0**(INISTP-ISTEP+1) )
      ENDIF
*
      OFIRST = .FALSE.
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CALCDT    !! calculate time step (CFL)
     O           ( DELTX ,
     I             DDELT , GDU   , GDV   , 
     I             DTMIN , DTLCM , SAFER   )
*
*   [PARAM] 
#include     "zcdim.F"
#include     "zccom.F"
*
*   [OUTPUT] 
      REAL*8     DELTX                       !! time step
*
*   [INPUT] 
      REAL*8     DDELT                       !! standard time step
      REAL*8     GDU   ( IJDIM*KMAX )        !! zonal wind
      REAL*8     GDV   ( IJDIM*KMAX )        !! meridional wind
      REAL*8     DTMIN                       !! minimum time step
      REAL*8     DTLCM                       !! (minimum)common multiple
      REAL*8     SAFER                       !! safty factor
*
*   [INTERNAL WORK] 
      INTEGER    IJK, IDIV, JDIV
      REAL*8     DELTMX, VSM, VSMAX
      INTEGER    I, J, K, II, JJ, KK
*
*   [INTERNAL SAVE] 
      REAL*8     DELTSV
      REAL*8     VSMIN
      LOGICAL    OFIRST
      DATA       DELTSV / 0. /
      DATA       VSMIN  / 1. /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         DELTSV = DDELT
      ENDIF
*
      VSMAX = 0.
      II = 0
      JJ = 0
      KK = 0
      IJK = 0
      DO 1100 K = 1, KMAX
         DO 1100 J = 1, JDIM
            DO 1100 I = 1, IDIM
               IJK = IJK + 1
               VSM = VSMAX
               VSMAX = MAX( VSMAX, GDU( IJK )**2+GDV( IJK )**2 )
               IF( VSM .NE. VSMAX ) THEN
                  II = I
                  JJ = J
                  KK = K
               ENDIF
 1100 CONTINUE 
      WRITE (6,*) '**** MAX.EDDY-WIND :', SQRT(VSMAX),' AT:',II,JJ,KK
*
      DELTMX = SAFER * ER / MAX( NMAX, 1 ) / MAX( SQRT(VSMAX), VSMIN )
*
      IF ( DELTMX .GT. DDELT ) THEN
         DELTX = DDELT
      ELSE
         IDIV   = INT( DTLCM/DELTMX ) + 1 

 2100    CONTINUE
            DELTX = DTLCM/IDIV
            JDIV  = NINT(DELTX/DTMIN)
            IF ( (JDIV .GE. 1) .AND. 
     &           (NINT( DTMIN*JDIV - DELTX ) .NE. 0)  ) THEN
               IDIV = IDIV+1 
               GOTO 2100
            ENDIF
*
         DELTX = MAX( DELTX, DTMIN )
      ENDIF
*
      IF ( DELTX .NE. DELTSV ) THEN
         WRITE (6,*) '**** TIME STEP CHANGED : ', DELTX
         WRITE (6,*) '**** MAXIMUM WIND      : ', SQRT(VSMAX)
      ENDIF
      DELTSV = DELTX
*
      RETURN
      END
