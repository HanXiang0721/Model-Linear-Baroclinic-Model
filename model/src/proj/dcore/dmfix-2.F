* PACKAGE DMFIX !! mass fixer & negative tracer removal
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/06(numaguti) change FIXMAS for accurate restart
*********************************************************************
      SUBROUTINE MASFIX   !! mass fixer & negative tracer removal
     M         ( GDPS  , GDQ   , 
     I           DELT            )
*
*      globally conserve & remove negative
*
*   [PARAM] 
#include        "zcdim.F"                 /* # of grid point & wave */
#include        "zpdim.F"                 /* physics etc.           */
#include        "zcord.F"                 /* coordinate             */
*
*   [MODIFY]
      REAL*8     GDPS   ( IJDIM )             !! surface pressure
      REAL*8     GDQ    ( IJDIM, KMAX, NTR )  !! humidity q
*
*   [INPUT]     
      REAL*8     DELT
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, IT, IFPAR, JFPAR, ITER
      REAL*8     TM, QP, TQ, DTQ, TDM
      REAL*8     RATIO
      REAL*8     DELI
*
*   [INTERNAL SAVE] 
      REAL*8     DRYMAS
      LOGICAL    OTRPOS( NTR )
      LOGICAL    OQLPOS
      INTEGER    NITER
      REAL*8     TMAS
      REAL*8     TQMAS ( NTR )
      REAL*8     TDMAS
      LOGICAL    OFIRST
      NAMELIST  /NMMFIX/ DRYMAS, OTRPOS, OQLPOS, NITER
      DATA       DRYMAS / -999.      /
      DATA       OTRPOS / NTR*.TRUE. /
      DATA       OQLPOS / .FALSE.    /
      DATA       NITER  / 2          /
      DATA       TMAS   / 0.         /
      DATA       TQMAS  / NTR*0.     /
      DATA       TDMAS  / 0.         /
      DATA       OFIRST / .TRUE.     /
*     
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' ### MASFIX : FIXMAS MUST BE CALLED FIRST ###'
         CALL XABORT(1)
      ENDIF
*
      DELI = 1.D0/(2.D0*DELT)
      CALL HISTIF
     I   ( GDQ , 'SRMNQ', 'moisture fixer','1/s', 'ALEV', -DELI )
      CALL HISTIF
     I   ( GDPS, 'MFIX ', 'mass fixer',  'hPa/s', 'ASFC', -DELI )
*
*  << total mass >>
*
      TM = 0.D0
      DO 1100 IJ = 1, IJDIM
         TM = TM + GDPS( IJ )*DLON( IJ )*DLAT( IJ )
 1100 CONTINUE 
      RATIO = TMAS/TM 
      DO 1200 IJ = 1, IJDIM
         GDPS( IJ ) = RATIO * GDPS( IJ )
 1200 CONTINUE 
*
      IF ( ABS(1.D0-RATIO) .GT. 0.01D0 ) THEN
         WRITE (6,*) 
     &         ' ### MASFIX: BAD CONSERVATION OF TOT MASS ### ', RATIO
      ENDIF
*     
*  << tracers : local >>
*
      DO 2200 IT = 1, NTR
         IF ( OTRPOS( IT ) ) THEN
            CALL HORNRM   !! horizontal (local) negative tracer removal
     M         ( GDQ(1,1,IT),
     I           GDPS  , DLON  , DLAT  )
         ENDIF
 2200 CONTINUE 
*
      CALL HISTAD ( GDQ         , 'SRMNQ',  DELI )
      CALL HISTAD ( GDPS        , 'MFIX ',  DELI )
*
      RETURN
*===================================================================
      ENTRY      FIXMAS        !! mass fixer
     M         ( GDPS  , 
     I           GDQ      )
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DMFIX: MASS FIXER 96/06/05'
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMMFIX, END=190 )
  190    WRITE  ( JFPAR, NMMFIX )
         IF ( ITL .GE. 2 ) OTRPOS( ITL ) = OQLPOS 
      ELSE
*     
*  << dry mass fixer >>
*
         DO 3000 ITER = 1, NITER
            CALL TOTMAS         !! summation of total mass
     O         ( TMAS  , TDM   , TQMAS ,
     I           GDPS  , GDQ   ,
     I           DLON  , DLAT  , DSIG   )
*
            RATIO = TDMAS/TDM 
            DO 3300 IJ = 1, IJDIM
               GDPS( IJ ) = GDPS( IJ ) * RATIO
 3300       CONTINUE 
*
            IF ( ABS(1.D0-RATIO) .GT. 0.01D0 ) THEN
               WRITE (6,*) 
     &      ' ### FIXMAS: BAD CONSERVATION OF DRY MASS ### ', RATIO
            ENDIF
 3000    CONTINUE 
*
      ENDIF
*
      CALL TOTMAS         !! summation of total mass
     O         ( TMAS  , TDMAS , TQMAS ,
     I           GDPS  , GDQ   ,
     I           DLON  , DLAT  , DSIG   )
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         WRITE (6,*) ' *** FIXMAS : TDMAS = ', TDMAS
      ENDIF
*
      IF ( DRYMAS .GT. 0 ) THEN
         TDMAS = DRYMAS
      ENDIF
*
      RETURN
      END
*********************************************************************
      SUBROUTINE HORNRM    !! horizontal (local) negative tracer removal
     M         ( GDQ   ,
     I           GDPS  , DLON  , DLAT  )
*
*   [PARAM] 
#include        "zcdim.F"                 /* # of grid point & wave */
*
*   [MODIFY]
      REAL*8     GDQ    ( IDIM, JDIM, KMAX )  !! humidity q
*
*   [INPUT]     
      REAL*8     GDPS   ( IDIM, JDIM )        !! surface pressure
      REAL*8     DLON   ( IDIM, JDIM )
      REAL*8     DLAT   ( IDIM, JDIM )
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K, IP, IM, JP, JM
      REAL*8     DSX, DSP, DSM, SUMQ, DELQ
      REAL*8     RATIO
*
      DO 2210 K = 1, KMAX
         DO 2210 J = 1, JMAX
            JP  = J + 1
            JM  = J - 1
            DSX = DLON( 1,J )*DLAT( 1,J )
            IF ( JP .GT. JMAX ) THEN
               JP  = JMAX
               DSP = 0.
            ELSE
               DSP = DLON( 1,JP )*DLAT( 1,JP )
            ENDIF
            IF ( JM .LT. 1 ) THEN
               JM  = 1
               DSM = 0.
            ELSE
               DSM = DLON( 1,JM )*DLAT( 1,JM )
            ENDIF
            DO 2220 I = 1, IMAX
               IP = MOD( I,        IMAX )+1
               IM = MOD( I+IMAX-2, IMAX )+1
               IF ( GDQ( I,J,K ) .LT. 0. ) THEN
                  SUMQ = DSM*( MAX( GDQ( IM,JM,K ),0.D0 )
     &                        +MAX( GDQ( I ,JM,K ),0.D0 )
     &                        +MAX( GDQ( IP,JM,K ),0.D0 ) )
     &                 + DSX*( MAX( GDQ( IM,J ,K ),0.D0 )
     &                        +MAX( GDQ( IP,J ,K ),0.D0 ) )
     &                 + DSP*( MAX( GDQ( IM,JP,K ),0.D0 )
     &                        +MAX( GDQ( I ,JP,K ),0.D0 )
     &                        +MAX( GDQ( IP,JP,K ),0.D0 ) )
                  DELQ = MIN( - DSX*GDQ( I,J,K ), SUMQ )
                  IF ( DSX .GT. 0.D0 ) THEN
                     GDQ( I,J,K ) = GDQ( I,J,K ) + DELQ/DSX
                  ENDIF
                  IF ( SUMQ .GT. 0.D0 ) THEN
                     RATIO = DELQ/SUMQ
                  ELSE
                     RATIO = 0.D0
                  ENDIF
                  IF ( DSM .GT. 0.D0 ) THEN
                     GDQ( IM,JM,K ) = GDQ( IM,JM,K )
     &                              - RATIO*MAX( GDQ( IM,JM,K ),0.D0 )
                     GDQ( I ,JM,K ) = GDQ( I ,JM,K )
     &                              - RATIO*MAX( GDQ( I ,JM,K ),0.D0 )
                     GDQ( IP,JM,K ) = GDQ( IP,JM,K )
     &                              - RATIO*MAX( GDQ( IP,JM,K ),0.D0 )
                  ENDIF
                  IF ( DSP .GT. 0. ) THEN
                     GDQ( IM,JP,K ) = GDQ( IM,JP,K )
     &                              - RATIO*MAX( GDQ( IM,JP,K ),0.D0 )
                     GDQ( I ,JP,K ) = GDQ( I ,JP,K )
     &                              - RATIO*MAX( GDQ( I ,JP,K ),0.D0 )
                     GDQ( IP,JP,K ) = GDQ( IP,JP,K )
     &                              - RATIO*MAX( GDQ( IP,JP,K ),0.D0 )
                  ENDIF
                  GDQ( IM,J ,K ) = GDQ( IM,J ,K )
     &                           - RATIO*MAX( GDQ( IM,J ,K ),0.D0 )
                  GDQ( IP,J ,K ) = GDQ( IP,J ,K )
     &                           - RATIO*MAX( GDQ( IP,J ,K ),0.D0 )
               ENDIF
 2220       CONTINUE 
 2210 CONTINUE
*
      RETURN
      END
********************************************************************
      SUBROUTINE TOTMAS         !! summation of total mass
     O         ( TMAS  , TDMAS , TQMAS ,
     I           GDPS  , GDQ   ,
     I           DLON  , DLAT  , DSIG   )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc.           */
*
*   [OUTPUT] 
      REAL*8     TMAS        
      REAL*8     TDMAS 
      REAL*8     TQMAS  ( NTR )
*
*   [INPUT]     
      REAL*8     GDPS   ( IDIM, JDIM )             !! surface pressure
      REAL*8     GDQ    ( IDIM, JDIM, KMAX, NTR )  !! humidity q
      REAL*8     DLON   ( IDIM, JDIM )
      REAL*8     DLAT   ( IDIM, JDIM )
      REAL*8     DSIG   ( KMAX )
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K, IT 
      REAL*8     TM, TQM, TQMA
*
      TMAS = 0.D0
      DO 7100 J = 1, JDIM
         TM = 0.D0
         DO 7110 I = 1, IDIM
            TM = TM + GDPS( I,J )*DLON( I,J )*DLAT( I,J )
 7110    CONTINUE 
         TMAS = TMAS + TM
 7100 CONTINUE 
*
      TDMAS = TMAS
      DO 7200 IT = 1, NTR
         TQMAS( IT ) = 0.D0
         DO 7210 K = 1, KMAX
            TQMA = 0.D0
            DO 7220 J = 1, JDIM
               TQM  = 0.D0
               DO 7230 I = 1, IDIM
                  TQM = TQM
     &                + GDQ( I,J,K,IT )
     &                * GDPS( I,J )*DLON( I,J )*DLAT( I,J )*DSIG( K )
 7230          CONTINUE 
               TQMA = TQMA + TQM
 7220       CONTINUE 
            TQMAS( IT ) = TQMAS( IT ) + TQMA
 7210    CONTINUE 
         IF ( IT .LE. ITL ) THEN
            TDMAS = TDMAS - TQMAS( IT )
         ENDIF
 7200 CONTINUE
*
      RETURN
      END
