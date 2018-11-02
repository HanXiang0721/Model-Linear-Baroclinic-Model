* PACKAGE DVTMP !! dynamics: virtual temperature
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
***********************************************************************
      SUBROUTINE VIRTMD    !! virtual temperature
     O         ( GDTV  ,
     I           GDT   , GDQ     )
*
*   [PARAM] 
#include        "zcdim.F"
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"
*
*   [OUTPUT]
      REAL*8     GDTV  ( IJDIM, KMAX )   !! virtual temperature
*
*   [INPUT]
      REAL*8     GDT   ( IJDIM, KMAX       )  !! temperature T
      REAL*8     GDQ   ( IJDIM, KMAX       )  !! humidity
*
*   [INTERNAL WORK]
      INTEGER    IJ, K
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DVTMP: VIRTUAL TEMP. 93/11/08'
         OFIRST = .FALSE.
      ENDIF
*
      DO 1200 K = 1, KMAX
         DO 1200 IJ = 1, IJDIM
            GDTV( IJ,K ) = GDT( IJ,K )
     &           *( 1.+ EPSVT*GDQ( IJ,K ) )
 1200 CONTINUE
*
      RETURN
      END
