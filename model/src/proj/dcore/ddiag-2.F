* PACKAGE DDIAG  !! dynamics: diagnosis
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/05/28(numaguti) restructure
*         01/07/05(hiro)     omega
*
***********************************************************************
      SUBROUTINE DDIAG     !!  diagnostic output
     I         ( GDU   , GDV   , GDT   , GDPS  , GDQ   ,
     I           GDVOR , GDDIV , GDTV  , GSIGD , GPID  ,
     I           GDW   , GDBI  , GDTH  , GDZS  , TFLX  , 
     I           OANM  , 
     W           GDX   , GDZ                            )
*
#include        "zcdim.F"                /* # of grid point & wave  */
#include        "zpdim.F"                /* # of physics grid point */
#include        "zccom.F"                /* stand. physical const.  */
#include        "zcord.F"                /* coordinate              */
*
*   [INPUT]
      REAL*8     GDU   ( IJDIM, KMAX )      !! westerly U
      REAL*8     GDV   ( IJDIM, KMAX )      !! southern wind V
      REAL*8     GDT   ( IJDIM, KMAX )      !! temperature T
      REAL*8     GDPS  ( IJDIM )            !! surface pressure
      REAL*8     GDQ   ( IJDIM, KMAX, NTR ) !! humidity q
      REAL*8     GDVOR ( IJDIM, KMAX )      !! vorticity
      REAL*8     GDDIV ( IJDIM, KMAX )      !! divergence
      REAL*8     GDTV  ( IJDIM, KMAX )      !! virtual temperature
      REAL*8     GDW   ( IJDIM, KMAX )      !! p-velocity
      REAL*8     GDBI  ( IJDIM, KMAX )      !! baroclinicity
      REAL*8     GDTH  ( IJDIM, KMAX )      !! potential temp
      REAL*8     GSIGD ( IJDIM, KMAX+1 )    !! tendency of sigma
      REAL*8     GPID  ( IJDIM  )           !! tendency of pai
      REAL*8     GDZS  ( IJDIM  )           !! surface topography
      REAL*8     TFLX  ( IJDIM, KMAX )      !! Newtonian cooling
      LOGICAL    OANM                       !! anomaly output?
*
*   [WORK] 
      REAL*8     GDX   ( IDIM*JDIM, KMAX+1 )
      REAL*8     GDZ   ( IDIM*JDIM, KMAX   )
*
*   [INTERNAL SAVE]
      REAL*8     GFZ  ( IJDIM, KMAX )       !! geopotential height
      REAL*8     GFW  ( IJDIM, KMAX )       !! p-velocity
      SAVE       GFZ, GFW
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, IFPAR, JFPAR
      REAL*8     TVM
      LOGICAL    OOUT
*
*   [INTERNAL PARM] 
      REAL*8     TLAPS                       !! lapse rate: underground
      LOGICAL    OFIRST
      NAMELIST  /NMSLP/ TLAPS
      DATA       TLAPS  /  0.005 /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DDIAG: DYNAMICS DIAGNOSTICS 96/05/28'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSLP, END=190 )
  190    WRITE  ( JFPAR, NMSLP )
         CALL GHYDRO            !! hydrostatic eq.
     O           ( GFZ   ,
     I             GDTV  , GDZS  )
         CALL COPY ( GFW , GDW , IJDIM*KMAX  )
      ENDIF
*
      CALL HISTIN
     I      ( GDVOR, 'VOR', 'vorticity     ' ,'1/s   ', 'ALEV'   )
      CALL HISTIN
     I      ( GDDIV, 'DIV', 'divergence    ' ,'1/s   ', 'ALEV'   )
*
      CALL HISTIN
     I      ( GSIGD, 'SIGD  ', 'sigma-velocity  ' ,'1/s   ', 'AMLEV' )
      CALL HISTIN
     I      ( GPID , 'PID   ', 'pi-tendency     ' ,'1/s   ', 'ASFC'  )
*
      IF ( OANM ) THEN
         DO 1100 K = 1, KMAX
            DO 1100 IJ = 1, IJDIM
               GDX( IJ,K ) 
     &              = GDW ( IJ,K ) - GFW( IJ,K ) 
 1100    CONTINUE
      ELSE
         DO 1150 K = 1, KMAX
            DO 1150 IJ = 1, IJDIM
               GDX( IJ,K ) = GDW ( IJ,K )
 1150    CONTINUE
      ENDIF
      CALL HISTIN
     I    ( GDX, 'OMGF',   'p-velocity  ', 'hPa/s', 'ALEV'   )

      CALL HISTIN
     I    ( GDBI, 'BI',   'baroclinicity', '1/s', 'ALEV'     )

      CALL HISTIN
     I    ( GDTH, 'THETA','potential temperature', 'K', 'ALEV' )
*
      CALL GHYDRO                       !! hydrostatic eq.
     O         ( GDZ   ,
     I           GDTV  , GDZS  )
      IF( OANM ) THEN
         DO 1200 K = 1, KMAX
            DO 1200 IJ = 1, IJDIM
               GDZ( IJ,K ) 
     &              = GDZ( IJ,K ) - GFZ( IJ,K )
 1200    CONTINUE
      ENDIF
      CALL HISTIN
     I    ( GDZ, 'Z',   'geopotential height', 'm', 'ALEV'   )
*
      CALL HISTRG
     I   ( OOUT,'SLP', 'sea level pressure', 'hPa', 'ASFC' )
      IF ( OOUT ) THEN
         DO 2100 IJ = 1, IJDIM
            TVM         = GDTV(IJ,1) + GDZS(IJ)*TLAPS/2.
            GDX( IJ,1 ) = GDPS(IJ)*EXP( GRAV*GDZS(IJ)/(RAIR*TVM) )
 2100    CONTINUE 
         CALL HISTAX( GDX, 'SLP', 1.D0, .FALSE. )
      ENDIF
*
      CALL HISTRG
     I   ( OOUT , 'UV', 'uv', '(m/s)**2', 'ALEV' )
      IF ( OOUT ) THEN
         DO 2200 K = 1, KMAX
            DO 2200 IJ = 1, IJDIM
               GDX( IJ,K ) = GDU( IJ,K )*GDV( IJ,K )
 2200    CONTINUE 
         CALL HISTAX( GDX, 'UV', 1.D0, .FALSE. )
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'UT', 'uT', 'K*m/s', 'ALEV' )
      IF ( OOUT ) THEN
         DO 2300 K = 1, KMAX
            DO 2300 IJ = 1, IJDIM
               GDX( IJ,K ) = GDU( IJ,K )*GDT( IJ,K )
 2300    CONTINUE 
         CALL HISTAX( GDX, 'UT', 1.D0, .FALSE. )
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'UQ', 'uq', 'K m/s', 'ALEV' )
      IF ( OOUT ) THEN
         DO 2400 K = 1, KMAX
            DO 2400 IJ = 1, IJDIM
               GDX( IJ,K ) = GDU( IJ,K )*GDQ( IJ,K,1 )
 2400    CONTINUE 
         CALL HISTAX( GDX, 'UQ', 1.D0, .FALSE. )
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'UZ', 'uz', 'm**2/s', 'ALEV' )
      IF ( OOUT ) THEN
         DO 2500 K = 1, KMAX
            DO 2500 IJ = 1, IJDIM
               GDX( IJ,K ) = GDU( IJ,K )*GDZ( IJ,K )
 2500    CONTINUE 
         CALL HISTAX( GDX, 'UZ', 1.D0, .FALSE. )
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'VT', 'vT', 'K*m/s', 'ALEV' )
      IF ( OOUT ) THEN
         DO 2600 K = 1, KMAX
            DO 2600 IJ = 1, IJDIM
               GDX( IJ,K ) = GDV( IJ,K )*GDT( IJ,K )
 2600    CONTINUE 
         CALL HISTAX( GDX, 'VT', 1.D0, .FALSE. )
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'VQ', 'vq', 'K m/s', 'ALEV' )
      IF ( OOUT ) THEN
         DO 2700 K = 1, KMAX
            DO 2700 IJ = 1, IJDIM
               GDX( IJ,K ) = GDV( IJ,K )*GDQ( IJ,K,1 )
 2700    CONTINUE 
         CALL HISTAX( GDX, 'VQ', 1.D0, .FALSE. )
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'VZ', 'vz', 'm**2/s', 'ALEV' )
      IF ( OOUT ) THEN
         DO 2800 K = 1, KMAX
            DO 2800 IJ = 1, IJDIM
               GDX( IJ,K ) = GDV( IJ,K )*GDZ( IJ,K )
 2800    CONTINUE 
         CALL HISTAX( GDX, 'VZ', 1.D0, .FALSE. )
      ENDIF
*
      CALL HISTRG
     I   ( OOUT,'PRECW','precipitable water','kg/m**2','ASFC')
      IF ( OOUT ) THEN
         CALL RESET( GDX, IJDIM*KMAX )
         DO 2900 K = 1, KMAX
            DO 2900 IJ = 1, IJDIM
               GDX( IJ,1 ) = GDX( IJ,1 )
     &                      + GDPS( IJ )*GDQ( IJ,K,1 )*100./GRAV
     &                                  *DSIG( K )
 2900    CONTINUE 
         CALL HISTAX( GDX, 'PRECW', 1.D0, .FALSE. )
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'KE ', 'kinetic energy      ' ,'J/m**2', 'ALEV'  )
      IF ( OOUT ) THEN
         DO 3100 K = 1, KMAX
            DO 3100 IJ = 1, IJDIM
               GDX ( IJ,K )
     &                = ( GDU( IJ,K )**2 + GDV( IJ,K )**2 )/ 2.D0
     &                  * GDPS( IJ ) * 100.D0 / GRAV
 3100    CONTINUE 
         CALL HISTAX( GDX, 'KE', 1.D0, .FALSE. )         
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'IE ', 'internal energy     ' ,'J/m**2', 'ALEV' )
      IF ( OOUT ) THEN
         DO 3200 K = 1, KMAX
            DO 3200 IJ = 1, IJDIM
               GDX ( IJ,K )
     &                = CP * GDT( IJ,K )
     &                  * GDPS( IJ ) * 100.D0 / GRAV
 3200    CONTINUE 
         CALL HISTAX( GDX, 'IE', 1.D0, .FALSE. )         
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'PE ', 'potential energy    ' ,'J/m**2', 'ALEV' )
      IF ( OOUT ) THEN
         DO 3300 K = 1, KMAX
            DO 3300 IJ = 1, IJDIM
               GDX ( IJ,K )
     &                = GDZ  ( IJ,K ) * GRAV
     &                  * GDPS( IJ ) * 100.D0 / GRAV
 3300    CONTINUE 
         CALL HISTAX( GDX, 'PE', 1.D0, .FALSE. )         
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'LE ', 'latent energy       ' ,'J/m**2', 'ALEV' )
      IF ( OOUT ) THEN
         DO 3400 K = 1, KMAX
            DO 3400 IJ = 1, IJDIM
               GDX ( IJ,K )
     &                = EL * GDQ( IJ,K,1 )
     &                  * GDPS( IJ ) * 100.D0 / GRAV
 3400    CONTINUE 
         CALL HISTAX( GDX, 'LE', 1.D0, .FALSE. )         
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'ENS', 'enstrophy     ' ,'kg/s**2/m**2', 'ALEV' )
      IF ( OOUT ) THEN
         DO 3500 K = 1, KMAX
            DO 3500 IJ = 1, IJDIM
               GDX ( IJ,K )
     &                = GDVOR ( IJ,K ) **2  
     &                  * GDPS( IJ ) * 100.D0 / GRAV
 3500    CONTINUE
         CALL HISTAX( GDX, 'ENS', 1.D0, .FALSE. )                  
      ENDIF
*
      CALL HISTRG
     I   ( OOUT, 'TM ', 'total mass         ' ,'kg/m**2', 'ASFC' )
      IF ( OOUT ) THEN
         DO 3600 IJ = 1, IJDIM
            GDX ( IJ,1 ) = GDPS ( IJ ) * 100.D0 / GRAV
 3600    CONTINUE
         CALL HISTAX( GDX, 'TM', 1.D0, .FALSE. )                  
      ENDIF
*
      CALL HISTIN
     I   ( TFLX,  'TFLUX' ,'Newtonian heating','K/s','ALEV' )
c      CALL HISTIF
c     I   ( QFLUX(1,1,1),'EVAP','latent heat flux','W/m**2','ASFC',EL )
*
      RETURN
      END
