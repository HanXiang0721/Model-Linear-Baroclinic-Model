* PACKAGE ATIME !! admin: control time 
*
*   [HIS] 96/07/05(numaguti) AGCM5.4f
**********************************************************************
      SUBROUTINE SETTIM    !! set time
     I         ( TIME   )
*
*   [INPUT] 
      REAL * 8   TIME                        !! time
*
*   [INTERNAL SAVE] 
      REAL * 8   TIMEZ                       !! saved time
      DATA       TIMEZ / 0.D0  /
*
      TIMEZ = TIME
*
      RETURN
*=====================================================================
      ENTRY      GETTIM    !! get time
     O         ( TIME   )
*
      TIME = TIMEZ
*
      RETURN
      END
