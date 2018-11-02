      PROGRAM REDIST
*
*     redistribution column groups obtained from 
*     1-step integration of AGCM
*     moisture included on 2001/06/15

      include 'dim.f'

      INTEGER     MSIZ0, MSIZ1
      REAL*8      MONE

      PARAMETER( MSIZ0 = 2*NMAX*(NVAR*KMAX+1) )
      PARAMETER( MSIZ1 = MSIZ0 * MMAX )
      PARAMETER( MONE = -1.D0 )

      REAL*8      DAT( MSIZ0 )
      REAL*8      OUT( MSIZ1 )
      REAL*8      UNT0( NVAR+1 ) !! unity for each variable
      REAL*8      UNT
      REAL*8      DATMIN
      INTEGER     I, II, N, K, L, L1, M, MM, M1
      INTEGER     LLMAX, MVAR, LEND
      INTEGER     KLEV( NVAR+1 )
*
      INTEGER    ICVAR
      INTEGER    ICLEV
      INTEGER    ICWAV
      INTEGER    ICWAVX
      INTEGER    ICLEN
      INTEGER    ICLENX
*
      CHARACTER   CVAR( NVAR+1 )
*
      INTRINSIC  DBLE, INDEX

      LOGICAL    OWALL        !! write all the wavenumber
      LOGICAL    OCLASSIC     !! classic dry model
      CHARACTER  CDR*90
      CHARACTER  CFO*90
      CHARACTER  CFI1*20, CFI2*20
      CHARACTER  CFILEI*90

      NAMELIST   /NMRED/   CDR, CFO
      NAMELIST   /NMALL/   OWALL
      NAMELIST   /NMCLS/   OCLASSIC

      DATA CVAR   /'v','d','t','p','q'/
      DATA KLEV   / KMAX, KMAX, KMAX, 1   , KMAX /
      DATA       UNT0 / 1.0D-7, 1.0D-7, 1.0D-3, 1.0D-5, 1.0D-6 /
      DATA       DATMIN / 0.D0 /
      DATA CFI1  
     $/'/MAT_**/AMATRIX    '/
      DATA CFI2
     $/'/MAT_***/AMATRIX   '/
      CHARACTER   CFILE*90
*       123456789012345678901234567890123456789012345678901234567890
      DATA        OWALL    / .FALSE. / !! PWM
      DATA        OCLASSIC / .TRUE.  / !! dry model
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMRED) 
      REWIND( 1 )
      READ( 1, NMALL) 
      REWIND( 1 )
      READ( 1, NMCLS) 
*
      ICVAR = 6
      ICLEV = 7
      ICWAV = 16
      ICLEN = INDEX( CDR, ' ' ) - 1
*
*     open output file
*
      OPEN( 10, FILE = CFO, FORM = 'UNFORMATTED', 
     &     STATUS = 'UNKNOWN' )
*
*     read data
*
      IF( OCLASSIC ) THEN
         MVAR = NVAR -1
      ELSE
         MVAR = NVAR
      ENDIF
      DO 100 M = 0, NTR         !! zonal wave number
         L = NMAX - M + 1
         IF( M .EQ. 0 ) THEN
            L = L - 1
         ELSE
            L = 2 * L
         ENDIF
         IF( OWALL ) THEN
            MM = MMAX 
         ELSE
            MM = 0
         ENDIF

         DO 10 N = 1, MVAR + 1  !! variable
            WRITE( CFI1 (ICVAR:ICVAR), '(A1)' ) CVAR( N )
            WRITE( CFI2 (ICVAR:ICVAR), '(A1)' ) CVAR( N )
            UNT = UNT0( N )
            DO 20 K = 1, KLEV( N ) !! level
               IF( K .LT. 10 ) THEN
                  WRITE( CFI1  (ICLEV:ICLEV), '(I1)' ) K
                  CFILE = CFI1
               ELSE
                  WRITE( CFI2 (ICLEV:ICLEV+1), '(I2)' ) K
                  CFILE = CFI2
               ENDIF
               ICWAVX = ICWAV
               IF( K .GE. 10 ) ICWAVX = ICWAVX + 1
               IF( M.LT.10 ) WRITE( CFILE (ICWAVX:ICWAVX), '(I1)' ) M
               IF( M.GE.10.AND.M.LT.100 ) 
     $              WRITE( CFILE (ICWAVX:ICWAVX+1), '(I2)' ) M
               IF( M.GE.100 ) 
     $              WRITE( CFILE (ICWAVX:ICWAVX+2), '(I3)' ) M

               CFILEI = CDR(1:ICLEN) // CFILE
               
               ICLENX = INDEX( CFILEI, ' ' ) - 1
               OPEN( 20, FILE = CFILEI, FORM = 'UNFORMATTED',
     $              STATUS = 'UNKNOWN', ERR=1999 )
               WRITE(6,*) 'INPUT:',CFILEI(1:ICLENX)
*     
               DO 40 L1 = 1, L
                  II = 0
                  DO 41 M1 = 0, MM
                     IF( OWALL ) THEN
                        LEND = MIN( LMAX, NMAX-M1)
                        IF( M1 .EQ. 0 ) THEN
                           LLMAX = LEND * ( MVAR * KMAX + 1 )
                        ELSE
                           LLMAX = 2 * ( LEND+1 ) * ( MVAR*KMAX + 1 )
                        ENDIF
                     ELSE
                        LLMAX = L * ( MVAR * KMAX + 1 )
                     ENDIF
                     
                     READ( 20, END=299, ERR=2999 ) (DAT(I),I=1,LLMAX)
                     IF( M1 .GT. NTR ) GOTO 41
                     DO 50 I = 1, LLMAX
                        II = II + 1
                        IF( DAT(I) .EQ. 0.D0 ) DAT(I) = DATMIN
                        OUT(II) = MONE * DAT(I) / UNT
 50                  CONTINUE
 41               CONTINUE
                  WRITE( 10 ) (OUT(I),I=1,II)
 40            CONTINUE
 299           CLOSE( 20 )
 20         CONTINUE
 10      CONTINUE
 100  CONTINUE
      GOTO 3999

 1999 WRITE( 6, *) ' #### FILE OPEN ERROR #### '
      CALL XABORT( 1 )
 2999 WRITE( 6, *) ' #### FILE READ ERROR #### '
      CALL XABORT( 1 )

 3999 CLOSE( 10 )

      STOP
      END
