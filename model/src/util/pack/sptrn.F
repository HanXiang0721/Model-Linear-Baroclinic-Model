* PACKAGE DSPTRN !" spherical trans.
**********************************************************************
      SUBROUTINE DSPTRI                  !" Triangular
     O         ( ALAT  , NMO   , W     , 
     O           NMDIM , IMAX  , JMAX  ,
     I           NMAX  , ISIZE           )
*
*   [OUTPUT] 
      REAL*8     ALAT  ( * )  !" ALAT( JMAX  )
      INTEGER    NMO   ( * )  !" NMO ( 2, 0:MMAX, 0:LMAX )
      REAL*8     W     ( * )  !" W   ( ISIZE )
      INTEGER    NMDIM
      INTEGER    IMAX
      INTEGER    JMAX
*   [INPUT] 
      INTEGER    NMAX
      INTEGER    ISIZE
*
*   [ENTRY INPUT] 
      REAL*8     GDATA ( * )  !" GDATA( IMAX, JMAX, KMAX )
      REAL*8     WDATA ( * )  !" WDATA( NMDIM, KMAX )
      INTEGER    KMAX
      CHARACTER  HGRAD *(*)   !" ' ', 'X', 'Y'
      REAL*8     WW    ( * )  !" WW   ( IMAX, JMAX, KMAX, 2 )
*
*   [INTERNAL SAVE]       
      INTEGER    IFAX  ( 10 )
      INTEGER    NTRIGS, NLAT, NPNM, NQPNM
      INTEGER    ITRIGS, ICOSLA, IQSINL, IGW, IQGW
      INTEGER    IPNM, IDPNM, IQPNM, IQDPNM, ISIZZ
      INTEGER    MMAX, LMAX, MINT, NML, NMDIM, JMXHF
      INTEGER    IMAXZ, JMAXZ, MMAXZ, LMAXZ, NMAXZ, NMDIMZ
      INTEGER    IPNMX, J
      REAL*8     PI
      SAVE
*
      JMAX   = (NMAX*3 + 2)/2
      IMAX   = JMAX*2
      MMAX   = NMAX
      LMAX   = NMAX
      MINT   = 1
      NML    = NMAX-LMAX
      NMDIM  = (MMAX/MINT+1)*(2*(NMAX+1)-LMAX)-NML*(NML+1)
      JMXHF  = JMAX/2+1
*
      IMAXZ  = IMAX
      JMAXZ  = JMAX
      MMAXZ  = NMAX
      LMAXZ  = NMAX
      NMAXZ  = NMAX
      NMDIMZ = NMDIM
*
      NTRIGS = IMAX*2 
      NPNM   = NMDIM*JMXHF
      NLAT   = JMAX
      NQPNM  = (NMAX+2)*(MMAX+1)
      ITRIGS = 1
      ICOSLA = ITRIGS + NTRIGS 
      IQSINL = ICOSLA
      IGW    = IQSINL + JMAX
      IQGW   = IGW
      IPNM   = IQGW   + JMAX
      IDPNM  = IPNM   + NPNM
      IQPNM  = IDPNM  + NPNM
      IQDPNM = IQPNM  + NQPNM
      ISIZZ  = IQDPNM + NQPNM 
*
#ifdef DEBUG
      WRITE (6,*) ' **** WORK AREA SIZE', ISIZZ, ISIZE
#endif      
      IF ( ISIZZ .GT. ISIZE ) THEN
         WRITE (6,*) ' **** WORK AREA IS TOO SMALL', ISIZZ, ISIZE
         STOP
      ENDIF
*
      IF ( IMAX .GE. 2 ) THEN
         CALL RFFTIM
     I      ( IMAX  ,
     O        W(ITRIGS), IFAX )
      ENDIF
      IF ( JMAX .GE. 2 ) THEN
         CALL GAUSS
     O         ( W(IQSINL), W(IQGW),
     I           JMAX                )
         CALL SETPNM
     O         ( W(IPNM)  , W(IDPNM), NMO   ,
     I           W(IQSINL),
     D           JMAX  , MMAX  , LMAX  , NMAX  , MINT  ,
     D           NMDIM , JMXHF ,
     W           W(IQPNM), W(IQDPNM)                     )
         IF ( IGW .NE. IQGW ) THEN
            DO 3100 J = 1, JMAX
               W(IGW+J-1) = W(IQGW+J-1)
 3100       CONTINUE 
         ENDIF
         DO 4100 J = 1, JMAX
            ALAT( J )     = ASIN( W(IQSINL+J-1) )
            W(ICOSLA+J-1) = SQRT( 1.D0 - W(IQSINL+J-1)**2 )
 4100    CONTINUE
      ENDIF
*
#ifdef DEBUG
      PI = ATAN(1.D0)*4.D0
      DO 100 J = 1, JMAX
         WRITE (6,*) ALAT(J)*180./PI
  100 CONTINUE 
      DO 110 J = 1, JMAX
         WRITE (6,*) W(IGW+J-1)
  110 CONTINUE 
#endif
*
      RETURN
*=====================================================================
      ENTRY      DSPW2G
     O         ( GDATA ,
     I           WDATA , KMAX  , HGRAD , 
     I           NMO   , W     , WW      )
*      
      IF ( HGRAD(1:1) .EQ. 'Y' ) THEN
         IPNMX = IDPNM
      ELSE
         IPNMX = IPNM
      ENDIF
      CALL SPW2G
     M         ( GDATA ,
     I           WDATA ,
     C           W(IPNMX), NMO   , W(ITRIGS), IFAX ,
     F           HGRAD   , 'POS' ,
     D           IMAXZ , JMAXZ , KMAX  , IMAXZ , JMAXZ ,
     D           LMAXZ , MMAXZ , NMAXZ , MINT  , NMDIMZ, JMXHF ,
     W           WW(1) , WW(IMAXZ*JMAXZ*KMAX+1)                 )
*
      RETURN
*=====================================================================
      ENTRY      DSPG2W
     O         ( WDATA ,
     I           GDATA , KMAX  , HGRAD , 
     I           NMO   , W     ,  WW     )
*      
      IF ( HGRAD(1:1) .EQ. 'Y' ) THEN
         IPNMX = IDPNM
      ELSE
         IPNMX = IPNM
      ENDIF
      CALL SPG2W
     M         ( GDATA ,
     I           WDATA ,
     C           W(IPNMX), NMO   , W(ITRIGS), IFAX , W(IGW),
     F           HGRAD   , 'POS' ,
     D           IMAXZ , JMAXZ , KMAX  , IMAXZ , JMAXZ ,
     D           LMAXZ , MMAXZ , NMAXZ , MINT  , NMDIMZ, JMXHF ,
     W           WW(1) , WW(IMAXZ*JMAXZ*KMAX+1)                 )
*
      RETURN
      END
***********************************************************************
      SUBROUTINE RESET     !" reset matrix to 0
     O         ( DATA  ,
     D           IDIM   )
*
*   [PARAM] 
      INTEGER    IDIM
*
*   [OUTPUT]       
      REAL*8     DATA ( IDIM )
*
*   [INTERNAL WORK] 
      INTEGER    I
*
      DO 1100 I = 1, IDIM
         DATA ( I ) = 0.  
 1100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE COPY      !" copy matrix
     O         ( DATAO  ,
     I           DATAI  ,
     D           IDIM     )
*
*   [PARAM]
      INTEGER    IDIM
*
*   [OUTPUT]       
      REAL*8     DATAO ( IDIM )                !" output data
*
*   [INPUT] 
      REAL*8     DATAI ( IDIM )                !" input data
*
*   [INTERNAL WORK] 
      INTEGER    I
*
      DO 1100 I = 1, IDIM
        DATAO ( I ) = DATAI ( I )
 1100 CONTINUE
*
      RETURN
      END
************************************************************************
      SUBROUTINE XABORT    !" abnormal end
     I         ( LEV    )
*
*   [INPUT] 
      INTEGER    LEV             !" error level
*
      WRITE (6,*) ' ###########: STOPPED DUE TO ERROR LEVEL=', LEV
      STOP
*
      END
