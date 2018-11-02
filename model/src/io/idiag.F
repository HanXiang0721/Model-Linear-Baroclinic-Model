* PACKAGE IDIAG !! i/o monitor
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
************************************************************************
      SUBROUTINE QDIAGP    !! refer prognostic point
     O         ( ODIAG , JFLDIA, NCOLX ,
     O           ISTR  , IEND  , IINT  ,
     I           KDIMD                          )
*
*   [PARAM] 
#include        "zcdim.F"                  /* # of grid point & wave */
#include        "zhdim.F"                  /* No. of character */
*
*   [OUTPUT] 
      LOGICAL    ODIAG
      INTEGER    JFLDIA
      INTEGER    NCOLX
      INTEGER    ISTR
      INTEGER    IEND
      INTEGER    IINT
*
*   [INPUT] 
      INTEGER    KDIMD
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
      INTEGER    IERR, KDIA0
      REAL * 8   TIME
      INTEGER    ITIME( 6 ) 
      CHARACTER  HTIME *20
*
*   [EXTERNAL FUNC] 
      LOGICAL    OINTVL
*
*   [INTERNAL SAVE] 
      INTEGER    JFILEZ
      REAL*8     TSTART, TPREV
      SAVE       JFILEZ, TSTART, TPREV
*
*   [INTERNAL PARM] 
      INTEGER    IDIAG
      INTEGER    JDIAG
      INTEGER    KDIAG
      CHARACTER  FILE  *(NFILN)
      REAL*8     TINTV                 !! interval
      CHARACTER  TUNIT  *(NCC)         !! unit
      INTEGER    START  ( 6 )          !! start output
      INTEGER    NCOLS
      LOGICAL    OFIRST
      NAMELIST  /NMDIAG/
     &          IDIAG, JDIAG, KDIAG, FILE, TINTV , TUNIT, START, NCOLS
      DATA       IDIAG  /  1  /
      DATA       JDIAG  /  1  /
      DATA       KDIAG  / -1  /
      DATA       FILE   / 'CON' /
      DATA       TINTV  / 10 /
      DATA       TUNIT  / 'DAY' /
      DATA       START  / -999, 1, 1, 0, 0, 0 /
      DATA       NCOLS  / 132 /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE (6,*) ' @@@ IDIAG: DIAGNOSE OUTPUT 93/12/07'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMDIAG,  END=190 )
 190     WRITE  ( JFPAR, NMDIAG )
*
         CALL GETTIM( TIME )
         IF ( START(1) .GE. 0 ) THEN
            CALL CYH2SS( TSTART, START  )
         ELSE
            TSTART = TIME
         ENDIF
         TPREV = TSTART
*
         CALL IFLOPN
     O         ( JFILEZ, IERR  ,
     I           FILE  , 'WRITE', '(F)'  )
      ENDIF
*
      CALL GETTIM( TIME )
*
      ODIAG = OINTVL            !! time step passed ?
     I         ( TIME  , TPREV , TSTART, TINTV, TUNIT )
*
      IF ( .NOT. ODIAG ) RETURN
*
      JFLDIA = JFILEZ
      NCOLX  = NCOLS
*
*          < 2. ouput point >
*
      IF ( KDIAG .GT. 0 ) THEN
         KDIA0 = MIN( KDIAG, KDIMD )
      ELSE
         KDIA0 = KDIAG
      ENDIF
*
      IF      ( IDIAG .LE. 0 ) THEN
         ISTR  = 1 + IDIM*(ABS(JDIAG)-1) + IDIM*JDIM*(ABS(KDIA0)-1)
         IEND  = ISTR  + IDIM-1
         IINT  = MAX( -IDIAG, 1 )
      ELSE IF ( JDIAG .LE. 0 ) THEN
         ISTR  = IDIAG + IDIM*JDIM*(ABS(KDIA0)-1)
         IEND  = ISTR  + IDIM*JDIM-1
         IINT  = MAX( -JDIAG, 1 )*IDIM
      ELSE IF ( KDIA0 .LE. 0 ) THEN
         ISTR  = IDIAG + IDIM*(JDIAG-1)
         IEND  = IDIM*JDIM*KDIMD
         IINT  = MAX( -KDIA0, 1 )*IDIM*JDIM
      ELSE
         ISTR  = IDIAG + IDIM*(JDIAG-1) + IDIM*JDIM*(KDIA0-1)
         IEND  = ISTR
         IINT  = IDIM*JDIM*KDIMD
      ENDIF
      IF ( IEND  .GT. IDIM*JDIM*KDIMD ) IEND = IDIM*JDIM*KDIMD
*
*          < 3. print time >
*
      IF ( TIME .NE. TPREV ) THEN
         TPREV = TIME
         CALL CPERPO ( .FALSE. )
         CALL CSS2YH ( ITIME, TIME )
         CALL CPERPO ( .TRUE. )
         CALL CYH2CC ( HTIME, ITIME  )
*
         WRITE ( JFLDIA, * ) '==========[ TIME = ', HTIME, 
     &                       ']=========', IDIAG, JDIAG, KDIAG
      ENDIF
*
      RETURN
      END
*******************************************************************
      SUBROUTINE DIAGP     !! monitor of grid value
     I         ( GDATA , HTTL  , KDIMD , HFMT  )
*
*   [PARAM] 
#include      "zcdim.F"                  /* # of grid point & wave */
*
*   [INPUT] 
      REAL*8     GDATA ( * )                 !!
      CHARACTER  HTTL  *(*)
      INTEGER    KDIMD
      CHARACTER  HFMT  *(*)
*
*   [INTERNAL WORK] 
      LOGICAL    ODIAG
      INTEGER    JFLDIA, NCOLS, ISTR, IEND, IINT
*
      CALL QDIAGP
     O     ( ODIAG , JFLDIA, NCOLS,
     O       ISTR  , IEND  , IINT ,
     I       KDIMD                  )
*
      IF ( .NOT. ODIAG )       RETURN
      WRITE (6,*) HTTL, ':'
*
      CALL PRINTD
     I       ( JFLDIA, HFMT   , GDATA ,
     I         ISTR  , IEND   , IINT  , 1    ,
     I         IDIM*JDIM*KDIMD, 1     , NCOLS  )
*
      RETURN
      END
*******************************************************************
      SUBROUTINE PRINTD    !! print out
     I         ( JFILE, HFMT  , GDATA ,
     I           ISTR , IEND  , IINT  , JSEL ,
     I           IDIM , JDIM  , NCOLS          )
*
*   [PARAM] 
      INTEGER    IDIM, JDIM
*
*   [INPUT] 
      INTEGER    JFILE
      CHARACTER  HFMT  *(*)
      REAL*8     GDATA ( IDIM, JDIM )
      INTEGER    ISTR
      INTEGER    IEND
      INTEGER    IINT
      INTEGER    JSEL
      INTEGER    NCOLS
*
*   [INTERNAL WORK] 
      INTEGER    NCMAX
      PARAMETER ( NCMAX=300 )
      CHARACTER  HNUM   *30
      CHARACTER  HLINE *(NCMAX)
      INTEGER    NCH, I, IL, ILS, IL2
*
*   [EXTERNAL FUNC] 
      INTEGER    LENC
*
*
      WRITE ( HNUM , HFMT ) -9.9999E-10
      NCH  = LENC( HNUM )
      IF ( NCH+3 .GT. NCOLS ) THEN
         WRITE (6,*) ' ### PRINTD: TOO NARROW COLUMNS < ', NCH+3
      ENDIF
*
      HLINE = ' '
      ILS   = 1
      IL    = ILS
*
      DO 1100 I = ISTR, IEND, IINT
         WRITE ( HNUM, HFMT ) GDATA( I,JSEL )
         IL2 = IL+NCH-1
*
         IF ( IL2 .GT. NCOLS ) THEN
            WRITE ( JFILE, '(A)' ) HLINE(1:NCOLS)
            HLINE = ' '
            IL = ILS
            IL2 = IL+NCH-1
         ENDIF
*
         HLINE ( IL:IL2 ) = HNUM
         IL = IL2 + 1
 1100 CONTINUE
*
      WRITE ( JFILE, '(A)' ) HLINE(1:NCOLS)
*
      RETURN
      END
