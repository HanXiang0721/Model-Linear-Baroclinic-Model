* PACKAGE USUBS       !! etc.
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         97/08/01(numaguti) MOZ2IJ, IJ2MOZ
*
***********************************************************************
      SUBROUTINE RESET     !! reset matrix to 0
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
      SUBROUTINE IRESET     !! reset matrix to 0
     O         ( IDATA ,
     D           IDIM   )
*
*   [PARAM] 
      INTEGER    IDIM
*
*   [OUTPUT]       
      INTEGER    IDATA ( IDIM )
*
*   [INTERNAL WORK] 
      INTEGER    I
*
      DO 1100 I = 1, IDIM
         IDATA ( I ) = 0.  
 1100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE ICOPY     !! copy matrix
     O         ( IDATAO  ,
     I           IDATAI  ,
     D           IDIM     )
*
*   [PARAM]
      INTEGER    IDIM
*
*   [OUTPUT]       
      INTEGER    IDATAO ( IDIM )                !! output data
*     
*   [INPUT] 
      INTEGER    IDATAI ( IDIM )                !! input data
*
*   [INTERNAL WORK] 
      INTEGER    I
*
      DO 1100 I = 1, IDIM
        IDATAO ( I ) = IDATAI ( I )
 1100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE COPY      !! copy matrix
     O         ( DATAO  ,
     I           DATAI  ,
     D           IDIM     )
*
*   [PARAM]
      INTEGER    IDIM
*
*   [OUTPUT]       
      REAL*8     DATAO ( IDIM )                !! output data
*
*   [INPUT] 
      REAL*8     DATAI ( IDIM )                !! input data
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
***********************************************************************
      SUBROUTINE COPY2     !! copy matrix
     O         ( DATAO  ,
     I           DATAI  ,
     I           IDX    ,
     D           IDIM   , JDIM   )
*
*   [PARAM]
      INTEGER    IDIM, JDIM
*
*   [OUTPUT]       
      REAL*8     DATAO ( IDIM, JDIM )          !! output data
*
*   [INPUT] 
      REAL*8     DATAI ( IDIM )                !! input data
      INTEGER    IDX
*
*   [INTERNAL WORK] 
      INTEGER    I, J
*
      IF ( IDX .EQ. 0 ) THEN
         DO 1100 J = 1, JDIM
            DO 1100 I = 1, IDIM
               DATAO ( I,J ) = DATAI ( I )
 1100    CONTINUE
      ELSE
         DO 2100 I = 1, IDIM
            DATAO ( I,IDX ) = DATAI ( I )
 2100    CONTINUE
      ENDIF
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SETV      !! put scalar value to matrix
     O         ( DATA  ,
     I           VALUE ,
     D           IDIM   )
*
*   [PARAM] 
      INTEGER    IDIM
*
*   [OUTPUT] 
      REAL*8     DATA ( IDIM )                 !! matrix
*
*   [INPUT] 
      REAL*8     VALUE                         !! input data
*
*   [INTERNAL WORK] 
      INTEGER    I
*
      DO 1100 I = 1, IDIM
        DATA ( I ) = VALUE
 1100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE MADD      !! add matrix
     O         ( DATAO  ,
     I           DATAI  , DATAJ  ,
     D           IDIM              )
*
*   [PARAM]
      INTEGER    IDIM
*
*   [OUTPUT]       
      REAL*8     DATAO ( IDIM )                !! output data
*
*   [INPUT] 
      REAL*8     DATAI ( IDIM )                !! input data
      REAL*8     DATAJ ( IDIM )                !! input data
*
*   [INTERNAL WORK] 
      INTEGER    I
*
      DO 1100 I = 1, IDIM
        DATAO ( I ) = DATAI ( I ) + DATAJ ( I )
 1100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE MMLT     !! multiply matrix
     O         ( DATAO  ,
     I           DATAI  , DATAJ  ,
     D           IDIM              )
*
*   [PARAM]
      INTEGER    IDIM
*
*   [OUTPUT]       
      REAL*8     DATAO ( IDIM )                !! output data
*
*   [INPUT] 
      REAL*8     DATAI ( IDIM )                !! input data
      REAL*8     DATAJ ( IDIM )                !! input data
*
*   [INTERNAL WORK] 
      INTEGER    I
*
      DO 1100 I = 1, IDIM
        DATAO ( I ) = DATAI ( I ) * DATAJ ( I )
 1100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE FILLV     !! fill part of matrix with value
     M         ( DATA  ,
     I           VALUE ,
     D           IMIN  , IMAX  , IDIM  , JMIN  , JMAX  , JDIM  ,
     D           KMIN  , KMAX                                   )
*
*   [PARAM] 
      INTEGER    IDIM
      INTEGER    JDIM
      INTEGER    KMAX
*
*   [MODIFY] 
      REAL*8     DATA ( IDIM, JDIM, KMAX )   !! matrix data
*
*   [INPUT] 
      REAL*8     VALUE                       !! input data
      INTEGER    IMIN                        !! begin of first dimension
      INTEGER    IMAX                        !! end of first dimension
      INTEGER    JMIN                        !! begin of 2nd dimension
      INTEGER    JMAX                        !! end of second dimension
      INTEGER    KMIN                        !! begin of third dimension
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K
*
      DO 1100 K = KMIN, KMAX
        DO 1100 J = JMIN, JMAX
          DO 1100 I = IMIN, IMAX
            DATA ( I, J, K ) = VALUE
 1100 CONTINUE
*
*
      RETURN
      END
************************************************************************
      SUBROUTINE MAXMIN    !!  output max & min values in matrix
     I         ( DATA, IDIM, JDIM, KMAX, IMAX, JMAX, HITEM )
*
*   [PARAM] 
      INTEGER    IDIM
      INTEGER    JDIM
      INTEGER    KMAX
*
*   [INPUT] 
      REAL*8     DATA( IDIM, JDIM, KMAX )
      INTEGER    IMAX
      INTEGER    JMAX
      CHARACTER  HITEM*(*)                     !! title
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K
      INTEGER    IPMIN, JPMIN, KPMIN
      INTEGER    IPMAX, JPMAX, KPMAX
      REAL*8     DMIN, DMAX
*
      DMIN =  1.E36
      DMAX = -1.E36
      IPMIN = 0
      JPMIN = 0
      KPMIN = 0
      IPMAX = 0
      JPMAX = 0
      KPMAX = 0
*
      DO 1100 K=1,KMAX
        DO 1100 J=1,JMAX
          DO 1100 I=1,IMAX
*
            IF ( DATA( I,J,K ) .LT. DMIN ) THEN
              DMIN = DATA( I,J,K )
              IPMIN = I
              JPMIN = J
              KPMIN = K
            ENDIF
*
            IF ( DATA( I,J,K ) .GT. DMAX ) THEN
              DMAX = DATA( I,J,K )
              IPMAX = I
              JPMAX = J
              KPMAX = K
            ENDIF
*
 1100 CONTINUE
*
      WRITE (6,*) HITEM, ': min = ', DMIN,' at ', IPMIN,JPMIN,KPMIN
      WRITE (6,*) HITEM, ': max = ', DMAX,' at ', IPMAX,JPMAX,KPMAX
*
      RETURN
      END
************************************************************************
      SUBROUTINE CHKVAL    !!  output max & min values in matrix
     I         ( DATA  , IDIM  , JDIM  , KMAX  ,
     I           VALMIN, VALMAX, HITEM , HLABL , OALL  )
*
*   [PARAM] 
      INTEGER    IDIM
      INTEGER    JDIM
      INTEGER    KMAX
*
*   [INPUT] 
      REAL*8     DATA( IDIM, JDIM, KMAX )
      REAL*8     VALMIN                        !! appropriate minimum
      REAL*8     VALMAX                        !! appropriate maximum
      CHARACTER  HITEM*(*)                     !! title
      CHARACTER  HLABL*(*)                     !! title
      LOGICAL    OALL
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K
      INTEGER    IPMIN, JPMIN, KPMIN
      INTEGER    IPMAX, JPMAX, KPMAX
      REAL*8     DMIN, DMAX
*
      IPMIN = 0
      JPMIN = 0
      KPMIN = 0
      IPMAX = 0
      JPMAX = 0
      KPMAX = 0
*
      CALL RMXMN
     O      ( DMIN, DMAX,
     I        DATA, IDIM*JDIM*KMAX ) 
*
      IF ( OALL .OR. (DMIN.LT.VALMIN) .OR. (DMAX.GT.VALMAX) ) THEN
         DO 2100 K=1,KMAX
            DO 2100 J=1,JDIM
               DO 2100 I=1,IDIM
                  IF ( DATA( I,J,K ) .EQ. DMIN ) THEN
                     IPMIN = I
                     JPMIN = J
                     KPMIN = K
                  ENDIF
                  IF ( DATA( I,J,K ) .EQ. DMAX ) THEN
                     IPMAX = I
                     JPMAX = J
                     KPMAX = K
                  ENDIF
 2100    CONTINUE
      ENDIF
*
      IF ( DMIN .LT. VALMIN ) THEN
         WRITE (6,*) '** INVALID ', HITEM, '-min = ', DMIN,
     &               ' at ', IPMIN,JPMIN,KPMIN, ' in ', HLABL
      ELSE IF ( OALL ) THEN
         WRITE (6,*) '           ', HITEM, '-min = ', DMIN,
     &               ' at ', IPMIN,JPMIN,KPMIN, ' in ', HLABL
      ENDIF
*
      IF ( DMAX .GT. VALMAX ) THEN
         WRITE (6,*) '** INVALID ', HITEM, '-max = ', DMAX,
     &               ' at ', IPMAX,JPMAX,KPMAX, ' in ', HLABL
      ELSE IF ( OALL ) THEN
         WRITE (6,*) '           ', HITEM, '-max = ', DMAX,
     &               ' at ', IPMAX,JPMAX,KPMAX, ' in ', HLABL
      ENDIF
*
      RETURN
      END
************************************************************************
      SUBROUTINE RMXMN     !!  max & min values in matrix
     O         ( DMIN  , DMAX  ,
     I           DAT   , NDIM    )
*
*   [PARAM] 
      INTEGER    NDIM
*
*   [OUTPUT] 
      REAL*8     DMIN
      REAL*8     DMAX
*   [INPUT] 
      REAL*8     DAT ( NDIM )
*
*   [INTERNAL WORK] 
      INTEGER    I
*
      DMIN =  DAT(1)
      DMAX =  DAT(1)
*
      DO 1100 I=1,NDIM
         DMIN = MIN( DMIN, DAT( I ) )
         DMAX = MAX( DMAX, DAT( I ) )
 1100 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE MCUT      !! cut part of matrix
     O         ( GDCUT ,
     I           GDGLB , IJS   , IJSDIM, IJDIM , KLEVS )
*
*   [PARAM] 
      INTEGER    IJSDIM, IJDIM, KLEVS
*
*   [OUTPUT] 
      REAL*8     GDCUT ( IJSDIM, KLEVS )    !! extracted data
*
*   [INPUT] 
      REAL*8     GDGLB ( IJDIM,  KLEVS )    !! whole data
      INTEGER    IJS                        !! IJ index
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, IJ0
*      
      DO 1100 K = 1, KLEVS
         DO 1100 IJ = 1, IJSDIM
            IJ0 = IJ + IJS -1
            GDCUT ( IJ,K ) = GDGLB ( IJ0,K )
 1100 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE MPASTE    !! paste part of matrix
     M         ( GDGLB ,
     I           GDCUT , IJS   , IJSDIM, IJDIM , KLEVS )
*
*   [PARAM] 
      INTEGER    IJSDIM, IJDIM, KLEVS
*
*   [MODIFY] 
      REAL*8     GDGLB ( IJDIM,  KLEVS )    !! whole data
*
*   [INPUT] 
      REAL*8     GDCUT ( IJSDIM, KLEVS )    !! extracted data
      INTEGER    IJS                        !! IJ index
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, IJ0
*      
      DO 1100 K = 1, KLEVS
         DO 1100 IJ = 1, IJSDIM
            IJ0 = IJ + IJS -1
            GDGLB ( IJ0,K ) = GDCUT ( IJ,K )
 1100 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE MCUTI     !! cut part of matrix (integer)
     O         ( IDCUT ,
     I           IDGLB , IJS   , IJSDIM, IJDIM , KLEVS )
*
*   [PARAM] 
      INTEGER    IJSDIM, IJDIM, KLEVS
*
*   [OUTPUT] 
      INTEGER    IDCUT ( IJSDIM, KLEVS )    !! extracted data
*
*   [INPUT] 
      INTEGER    IDGLB ( IJDIM,  KLEVS )    !! whole data
      INTEGER    IJS                        !! IJ index
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, IJ0
*      
      DO 1100 K = 1, KLEVS
         DO 1100 IJ = 1, IJSDIM
            IJ0 = IJ + IJS -1
            IDCUT ( IJ,K ) = IDGLB ( IJ0,K )
 1100 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE MPASTI    !! paste part of matrix (integer)
     M         ( IDGLB ,
     I           IDCUT , IJS   , IJSDIM, IJDIM , KLEVS )
*
*   [PARAM] 
      INTEGER    IJSDIM, IJDIM, KLEVS
*
*   [MODIFY] 
      INTEGER    IDGLB ( IJDIM,  KLEVS )    !! whole data
*
*   [INPUT] 
      INTEGER    IDCUT ( IJSDIM, KLEVS )    !! extracted data
      INTEGER    IJS                        !! IJ index
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, IJ0
*      
      DO 1100 K = 1, KLEVS
         DO 1100 IJ = 1, IJSDIM
            IJ0 = IJ + IJS -1
            IDGLB ( IJ0,K ) = IDCUT ( IJ,K )
 1100 CONTINUE 
*
      RETURN
      END
***********************************************************************
      SUBROUTINE MOZ2IJ     !! (I,J,M)->(II,JJ)
     O         ( DIJ   ,
     I           DMOZ  ,
     I           IDIM  , JDIM  , KDIM  , 
     I           IMOZ  , JMOZ           )
*
*   [PARAM] 
      INTEGER    IDIM, JDIM, KDIM
      INTEGER    IMOZ, JMOZ
*
*   [OUTPUT]       
      REAL*8     DIJ  ( IDIM*IMOZ, JDIM*JMOZ, KDIM  )
*
*   [INPUT] 
      REAL*8     DMOZ ( IDIM, JDIM, KDIM, IMOZ*JMOZ )
*
*   [INTERNAL WORK] 
      INTEGER    I, J, IM, JM, II, JJ, M, K
*
      DO 1000 K = 1, KDIM
         DO 1100 JM = 1, JMOZ
            DO 1100 IM = 1, IMOZ
               M = (JM-1)*IMOZ + IM
               DO 1200 J = 1, IDIM
                  JJ = (J-1)*JMOZ + JM
                  DO 1200 I = 1, JDIM
                     II = (I-1)*IMOZ + IM
                     DIJ( II,JJ,K ) = DMOZ( I,J,K,M )
 1200          CONTINUE 
 1100    CONTINUE
 1000 CONTINUE 
*
      RETURN
      END
***********************************************************************
      SUBROUTINE IJ2MOZ     !! !! (II,JJ)->(I,J,M)
     O         ( DMOZ  ,
     I           DIJ   ,
     I           IDIM  , JDIM  , KDIM  , 
     I           IMOZ  , JMOZ           )
*
*   [PARAM] 
      INTEGER    IDIM, JDIM, KDIM
      INTEGER    IMOZ, JMOZ
*
*   [OUTPUT]       
      REAL*8     DMOZ ( IDIM, JDIM, KDIM, IMOZ*JMOZ )
*
*   [INPUT] 
      REAL*8     DIJ  ( IDIM*IMOZ, JDIM*JMOZ, KDIM  )
*
*   [INTERNAL WORK] 
      INTEGER    I, J, IM, JM, II, JJ, M, K
*
      DO 1000 K = 1, KDIM
         DO 1100 JM = 1, JMOZ
            DO 1100 IM = 1, IMOZ
               M = (JM-1)*IMOZ + IM
               DO 1200 J = 1, IDIM
                  JJ = (J-1)*JMOZ + JM
                  DO 1200 I = 1, JDIM
                     II = (I-1)*IMOZ + IM
                     DMOZ( I,J,K,M ) = DIJ( II,JJ,K )
 1200          CONTINUE 
 1100    CONTINUE
 1000 CONTINUE 
*
      RETURN
      END
**********************************************************************
      FUNCTION DGAUS       !! double Gauss
     I         ( DX     )
      REAL * 8   DGAUS
      REAL * 8   DX
*
      DGAUS=AINT(DX)+AINT(DX-AINT(DX)+1.d0)-1.d0
*
      END
************************************************************************
      SUBROUTINE XABORT    !! abnormal end
     I         ( LEV    )
*
*   [INPUT] 
      INTEGER    LEV             !! error level
*   [INTERNAL SAVE] 
      INTEGER    LEVSTP
      DATA       LEVSTP / 0 /
*
      IF ( LEV .GE. LEVSTP ) THEN
         WRITE (6,*) ' ###########: STOPPED DUE TO ERROR LEVEL=', LEV
         CALL YFINE
         STOP
      ELSE
         WRITE (6,*) ' ###########: ERROR BUT CONTINUE LEVEL=', LEV
      ENDIF
*
*======================================================================
      ENTRY      SABORT    !! set level of abnormal end
     I         ( LEV    )
      LEVSTP = LEV
      RETURN
      END
*******************************************************************
      SUBROUTINE RDTKNH    !! read character token
     M         ( HLINE,
     O           HTOKEN  )     
*
*   [MODIFY] 
      CHARACTER  HLINE   *(*)
*   [OUTPUT] 
      CHARACTER  HTOKEN *(*)
*
*   [INTERNAL WORK] 
      INTEGER    IL
*
      CALL CLADJ( HLINE )
      IL = INDEX(HLINE,' ')-1
      IF ( IL .GT. 0 ) THEN
         HTOKEN      = HLINE (1:IL)
         HLINE(1:IL) = ' ' 
      ELSE
         HTOKEN = HLINE
      ENDIF
*
      RETURN
      END
*******************************************************************
      SUBROUTINE RDTKNI    !! read integer token
     M         ( HLINE,
     O           ITOKEN )     
*
*   [MODIFY] 
      CHARACTER  HLINE   *(*)
*   [OUTPUT] 
      INTEGER    ITOKEN
*
*   [INTERNAL WORK] 
      INTEGER    IL
      CHARACTER  HFMT*5
*
      CALL CLADJ( HLINE )
      IL = INDEX(HLINE,' ')-1
      IF ( IL .GT. 0 ) THEN
         IF ( IL .LE. 9 ) THEN
            HFMT = '(I*)' 
            WRITE ( HFMT(3:3), '(I1)' ) IL
         ELSE
            HFMT = '(I**)' 
            WRITE ( HFMT(3:4), '(I2)' ) IL
         ENDIF
         READ  ( HLINE(1:IL), HFMT ) ITOKEN
         HLINE(1:IL) = ' ' 
      ELSE
         ITOKEN = 0
      ENDIF
*
      RETURN
      END
*******************************************************************
      SUBROUTINE RDTKNR    !! read floating token
     M         ( HLINE,
     O           RTOKEN )     
*
*   [MODIFY] 
      CHARACTER  HLINE   *(*)
*   [OUTPUT] 
      REAL*8     RTOKEN
*
*   [INTERNAL WORK] 
      INTEGER    IL
      CHARACTER  HFMT*7
*
      CALL CLADJ( HLINE )
      IL = INDEX(HLINE,' ')-1
      IF ( IL .GT. 0 ) THEN
         IF ( IL .LE. 9 ) THEN
            HFMT = '(F*.0)' 
            WRITE ( HFMT(3:3), '(I1)' ) IL
         ELSE
            HFMT = '(F**.0)' 
            WRITE ( HFMT(3:4), '(I2)' ) IL
         ENDIF
         READ  ( HLINE(1:IL), HFMT ) RTOKEN
         HLINE(1:IL) = ' ' 
      ELSE
         RTOKEN = 0.
      ENDIF
*
      RETURN
      END
***********************************************************************
      SUBROUTINE CATNUM   !! concatenate number to character
     O         ( HCHR  , 
     I           HCHR0 , N     , HDFMT )
*
*   [OUTPUT] 
      CHARACTER  HCHR  *(*)
*   [INPUT] 
      CHARACTER  HCHR0 *(*)
      INTEGER    N
      CHARACTER  HDFMT *(*)
*
*   [INTERNAL WORK] 
      CHARACTER  HNUM  *20
      INTEGER    IL, INUM
*   [EXTERNAL FUNC] 
      INTEGER    LENC
*
      WRITE ( HNUM, HDFMT ) N
      CALL CLADJ ( HNUM )
      IL    = LENC( HCHR0 )
      INUM  = LENC( HNUM )
      HCHR  = HCHR0(1:IL)//HNUM(1:INUM)
*
      RETURN
      END
***********************************************************************
*    the following routines are adopted from DCL written by M.SHIOTANI
***********************************************************************
      SUBROUTINE CLADJ     !! put characters to the left
     M         ( HCHR  )
*
*   [MODIFY] 
      CHARACTER  HCHR *(*)                   !! series of character
*
*   [INTERNAL WORK] 
      INTEGER    LC1, LC2, I
*
*   [EXTERNAL FUNC] 
      INTEGER    LENB
*
      LC1 = LEN  ( HCHR )
      LC2 = LENB ( HCHR )
*
      IF ( LC2 .NE. 0 ) THEN
         DO 1100 I = 1, LC1, 1
            IF ( I .LE. LC1-LC2 ) THEN
               HCHR(I:I) = HCHR(I+LC2:I+LC2)
            ELSE
               HCHR(I:I) = ' '
            END IF
 1100    CONTINUE
      END IF
*
      RETURN
      END
************************************************************************
      FUNCTION   LENB      !! No.of preceding blanks
     I         ( HC )
*
      INTEGER    LENB
*   [INPUT] 
      CHARACTER  HC   *(*)                   !! series of character
*
*   [INTERNAL WORK] 
      CHARACTER  HN *1, HS *1
      INTEGER    NN, N
*
      HN = CHAR(0)
      HS = ' '
      NN = LEN( HC ) - 1
*
      DO 1100 N = 1, LEN( HC )-1
         IF (  ( HC(N:N) .NE. HN ) .AND. ( HC(N:N) .NE. HS )  ) THEN
            NN = N-1
            GOTO 1200
         ENDIF
 1100 CONTINUE
*
 1200 CONTINUE
      LENB = NN
*
      END
************************************************************************
      FUNCTION   LENC      !! No.of characters except for blanks
     I         ( HC  )
*
      INTEGER    LENC
*   [INPUT] 
      CHARACTER  HC   *(*)                   !! series of character
*
*   [INTERNAL WORK] 
      CHARACTER  HN *1, HS *1
      INTEGER    NN, N
*
      HN = CHAR(0)
      HS = ' '
      NN = 1
*
      DO 1100 N = LEN( HC ), 2, -1
         IF (  ( HC(N:N) .NE. HN ) .AND. ( HC(N:N) .NE. HS )  ) THEN
            NN = N
            GO TO 1200
         ENDIF
 1100 CONTINUE
*
 1200 CONTINUE
      LENC = NN
*
      RETURN
      END
