* PACKAGE DSETC  !! dynamics: set constants (horizontal difference)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/06/15(numaguti) LMAX=0(x-z) model Laplacian
***********************************************************************
      SUBROUTINE DSETCO    !!  set latitudinal parameters (Coriolis..) 
     O         ( CORIOL  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     CORIOL ( IJDIM )            !! Corioris factor f
*   [ENTRY OUTPUT] 
      REAL*8     UVFACT ( IJDIM  )           !! factor of u -> U
*
*   [INTERNAL WORK] 
      INTEGER    IJ, IFPAR, JFPAR
      REAL*8     DDSEC
*
*   [INTERNAL SAVE] 
      REAL*8     OMEGA                       !! rotation angular vel.
      REAL*8     PI
      SAVE       OMEGA, PI
      REAL*8     DAYLEN                      !! length of a day
      CHARACTER  TUNIT*4                     !! unit of time
      REAL * 8   DSEC
      REAL*8     ALAT0D                      !! when JMAX=1(degree)
      LOGICAL    OFIRST 
      NAMELIST   /NMDAYL/ DAYLEN, TUNIT
      NAMELIST   /NMLATD/ ALAT0D
      DATA       DAYLEN / 24.    /
      DATA       TUNIT  / 'HOUR' /
      DATA       DSEC   / 0.     /
      DATA       ALAT0D / 0.0    /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSETC: SET COEFFICIENTS 93/11/05'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMDAYL, END=190 )
  190    WRITE  ( JFPAR, NMDAYL )
         CALL CXX2SS
     O         ( DDSEC ,
     I           DAYLEN, TUNIT, DSEC  )
         PI     = ATAN ( 1.D0 )*4.D0
         OMEGA  = 2.D0* PI / DDSEC
         IF ( JMAX .EQ. 1 ) THEN
            CALL   REWNML ( IFPAR , JFPAR )
            READ   ( IFPAR, NMLATD, END=290 )
  290       WRITE  ( JFPAR, NMLATD )
         ENDIF
      ENDIF
*
      IF ( JMAX .GT. 1 ) THEN
         DO 1100 IJ = 1, IJDIM
            CORIOL ( IJ ) = 2.D0*OMEGA * SIN( ALAT( IJ ) )
 1100    CONTINUE
      ELSE
         DO 1200 IJ = 1, IJDIM
            CORIOL ( IJ ) = 2.D0*OMEGA * SIN( ALAT0D*PI/180.D0 )
 1200    CONTINUE
      ENDIF
*
      RETURN
*=====================================================================
      ENTRY     DSETUV     !! conversion factor of U <-> u
     O        ( UVFACT )
*
      DO 2100 IJ = 1, IJDIM
         UVFACT ( IJ ) = COS( ALAT( IJ ) )
 2100 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE DSETLP    !! Laplacian factor
     O         ( FLAPLA )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     FLAPLA( NMDIM  )            !! Laplacian factor
*   [ENTRY OUTPUT] 
      REAL*8     EDEL  ( NMDIM  )            !! vor.,D -> U,V
*
*   [INTERNAL WORK] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
*
*   [INTERNAL WORK] 
      INTEGER    L, M, N, LEND
*
*   [INTERNAL PARM] 
      LOGICAL    OFIRST 
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSETC: LAPLACIAN FACTOR 95/06/15'
         OFIRST = .FALSE.
      ENDIF
*
      CALL DSETNM
     O        ( NMO    )
*
      IF ( LMAX .GT. 0 ) THEN
         DO 2110 M = 0 , MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
            DO 2100 L = 0 , LEND
               N = L + M
               IF ( N .GT. 0 ) THEN
                  FLAPLA( NMO(1,M,L) )= - DBLE(N*(N+1)) / ER**2
                  FLAPLA( NMO(2,M,L) )= - DBLE(N*(N+1)) / ER**2
               ENDIF
 2100       CONTINUE
 2110    CONTINUE
         FLAPLA( NMO(1,0,0) ) =  0.  
         FLAPLA( NMO(2,0,0) ) =  0.  
      ELSE
         DO 2200 M = 0 , MMAX, MINT
            FLAPLA( NMO(1,M,0) )= - DBLE( M**2 )/ ER**2
            FLAPLA( NMO(2,M,0) )= - DBLE( M**2 )/ ER**2
 2200    CONTINUE 
      ENDIF
*
      RETURN
*=============================================================
      ENTRY     DSETED     !! factors for spectral calculation
     O        ( EDEL   )
*
      CALL DSETNM
     O        ( NMO    )
*
      IF ( LMAX .NE. 0 ) THEN      
         DO 3110 M = 0 , MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
            DO 3100 L = 0 , LEND
               N = L + M
               IF ( N .GT. 0 ) THEN
                  EDEL  ( NMO(1,M,L) )= - ER / DBLE( N*(N+1) )
                  EDEL  ( NMO(2,M,L) )= - ER / DBLE( N*(N+1) )
               ENDIF
 3100       CONTINUE
 3110    CONTINUE
         EDEL  ( NMO(1,0,0) ) =  0.  
         EDEL  ( NMO(2,0,0) ) =  0.  
      ELSE
         DO 3200 M = 0 , MMAX, MINT
            IF ( M .GT. 0 ) THEN
               EDEL  ( NMO(1,M,0) )= - ER / DBLE( M**2 )
               EDEL  ( NMO(2,M,0) )= - ER / DBLE( M**2 )
            ENDIF
 3200    CONTINUE
         EDEL  ( NMO(1,0,0) ) =  ER
         EDEL  ( NMO(2,0,0) ) =  0.  
      ENDIF
*
      RETURN
      END
