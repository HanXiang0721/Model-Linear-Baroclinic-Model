* PACKAGE UCLOC  !! monitor of time
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
*******************************************************************
      SUBROUTINE CLCOUT    !! output of CPU time
*
*   [ENTRY INPUT] 
      CHARACTER  HTTL  *(*)
*
*   [INTERNAL WORK] 
      INTEGER    IC
      REAL*8     CPUT, VPUT
*
*   [INTERNAL SAVE] 
      INTEGER    NCLMAX
      PARAMETER  ( NCLMAX=100 )
      REAL*8     CPUTIM ( NCLMAX )
      REAL*8     VPUTIM ( NCLMAX )
      REAL*8     CPUOLD ( NCLMAX )
      REAL*8     VPUOLD ( NCLMAX )
      CHARACTER  HTITLE ( NCLMAX ) *16
      LOGICAL    OFIRST
      INTEGER    NCLOCK
      SAVE       CPUTIM, VPUTIM, CPUOLD, VPUOLD, HTITLE
*
      DATA       HTITLE / NCLMAX*' ' /
      DATA       OFIRST / .TRUE. /
      DATA       NCLOCK / 0 /
*
*
      WRITE ( 6,* ) '############## CPU TIME SUMMARY ################'
      DO 1100 IC = 1, NCLOCK
         IF ( HTITLE(IC) .NE. ' ' ) THEN
            WRITE ( 6, 1600 ) HTITLE(IC), CPUTIM(IC), VPUTIM(IC)
         ENDIF
 1100 CONTINUE
 1600 FORMAT ( ' ', A16, 2(1PE15.6) )
*
      CALL YCLOCK ( CPUT, VPUT )
      WRITE ( 6, 1600 ) ' TOTAL TIME = ', CPUT, VPUT
*
      RETURN
*====================================================================
      ENTRY      CLCSTR    !!  start measurement of cpu time
     I         ( HTTL   )
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL YCLOCL
      ENDIF
*
      CALL YCLOCK ( CPUT, VPUT )
*
      DO 2100 IC = 1, NCLOCK
         IF ( HTITLE( IC ) .EQ. HTTL ) THEN
            CPUOLD( IC ) = CPUT
            VPUOLD( IC ) = VPUT
                                               RETURN
         ENDIF
 2100 CONTINUE
*
      IF ( NCLOCK .LE. NCLMAX ) THEN
         NCLOCK = NCLOCK + 1
         HTITLE( NCLOCK ) = HTTL
         CPUOLD( NCLOCK ) = CPUT
         VPUOLD( NCLOCK ) = VPUT
         CPUTIM( NCLOCK ) = 0.
         VPUTIM( NCLOCK ) = 0.
      ENDIF
*
      RETURN
*====================================================================
      ENTRY      CLCEND    !!  finish measurement of cpu time
     I         ( HTTL    )
*
      CALL YCLOCK ( CPUT, VPUT )
*
      DO 3100 IC = 1, NCLOCK
         IF ( HTITLE( IC ) .EQ. HTTL ) THEN
            CPUTIM( IC ) = CPUTIM( IC ) + CPUT - CPUOLD( IC )
            VPUTIM( IC ) = VPUTIM( IC ) + VPUT - VPUOLD( IC )
                                               RETURN
         ENDIF
 3100 CONTINUE
*
      RETURN
      END
