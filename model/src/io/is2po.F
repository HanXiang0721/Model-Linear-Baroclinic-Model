* PACKAGE IS2PO   !! sigmga <->p conversion
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/21(numaguti) AGCM5.4f
*         96/12/04(numaguti) PSMLT
***********************************************************************
      SUBROUTINE S2PINT    !! coordinate transformation sigma -> p
     O         ( GDATAP, KMAXP , 
     I           GDATA , KMAXS , HCORD , RMISS ,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zidim.F"                /* No. of character */
#include        "zhdim.F"                /* No. of character */
      INTEGER    IDIMS, IDIME
      INTEGER    JDIMS, JDIME
*
*   [OUTPUT] 
      REAL*8     GDATAP( IDIMS:IDIME, JDIMS:JDIME, * )
      INTEGER    KMAXP
*
*   [INPUT] 
      REAL*8     GDATA ( IDIMS:IDIME, JDIMS:JDIME, * )
      INTEGER    KMAXS
      CHARACTER  HCORD *(*)
      REAL*8     RMISS
*
*   [ENTRY INPUT] 
      REAL*8     GDPS  ( IJDIM )
*
*   [INTERNAL WORK] 
      REAL*8     ASIG  ( NAXWRK )
      REAL*8     APLEV ( NAXWRK )
      INTEGER    IJS, IJDIMS, K
*
*   [INTERNAL SAVE]       
      REAL*8     GDPSZ ( IJDIM )      
      SAVE       GDPSZ
      LOGICAL    OSET 
      DATA       OSET  / .FALSE. /
*
      IF ( .NOT. OSET ) THEN
         WRITE ( 6,* ) ' ### IS2PO : PS IS NOT SET YET ### '
      ENDIF
*
      CALL HQCAXZ( ASIG,  HCORD  ,   1, KMAX  )
      CALL HQCDMZ( KMAXP, 'APLEV' )
      CALL HQCAXZ( APLEV, 'APLEV',   1, KMAXP )
*
      IJS    = (JDIMS-1)*IDIM+IDIMS
      IJDIMS = (IDIME-IDIMS+1)*(JDIME-JDIMS+1)
*
      CALL SPINTP
     O      ( GDATAP,
     I        GDATA , GDPSZ(IJS), ASIG  , 
     I        IJDIMS, KMAXS     , 
     I        APLEV , KMAXP     , RMISS   )
*
      RETURN
*=======================================================================
      ENTRY      P2SINT    !! coordinate transformation p -> sigma
     O         ( GDATA , KMAXS ,
     I           GDATAP, KMAXP , HCORD , 
     I           IDIMS , JDIMS , IDIME , JDIME   )
*
      IF ( .NOT. OSET ) THEN
         WRITE ( 6,* ) ' ### IS2PO : PS IS NOT SET YET ### '
      ENDIF
*
      CALL HQCAXZ( ASIG,  HCORD   )
      CALL HQCDMZ( KMAXS, HCORD   )
      CALL HQCAXZ( APLEV, 'APLEV' )
*
      IJS    = (JDIMS-1)*IDIM+IDIMS
      IJDIMS = (IDIME-IDIMS+1)*(JDIME-JDIMS+1)
*
      CALL PSINTP
     O      ( GDATA,
     I        GDATAP, GDPSZ(IJS), ASIG  , 
     I        IJDIMS, KMAXS , 
     I        APLEV , KMAXP               )
*
      RETURN
*=======================================================================
      ENTRY      SPSMLT    !! multiply Ps
     O         ( GDATAP,
     I           GDATA , KMAXS , 
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
      IJS    = (JDIMS-1)*IDIM+IDIMS
      IJDIMS = (IDIME-IDIMS+1)*(JDIME-JDIMS+1)
*
      DO 3100 K = 1, KMAXS
         CALL MMLT
     O      ( GDATAP(IDIMS,JDIMS,K), 
     I        GDATA (IDIMS,JDIMS,K), GDPSZ(IJS), IJDIMS )
 3100 CONTINUE 
*
      RETURN
*=======================================================================
      ENTRY      SETPSF    !! set surface pres. for transform to p-level
     I         ( GDPS   )
*
      OSET = .TRUE.
*
      CALL COPY ( GDPSZ, GDPS , IJDIM )
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SPINTP    !! coordinate transformation sigma -> p
     O         ( GDZP  ,
     I           GDZ   , GDPS  , SIG   , 
     I           IJDIMS, KLEV  , 
     I           PLEV  , KPLEV , RMISS   )
*
#include        "zcdim.F"                /* # of grid point & wave */
*   [PARAM] 
      INTEGER    IJDIMS
      INTEGER    KLEV
      INTEGER    KPLEV
*
*   [OUTPUT] 
      REAL*8     GDZP ( IJDIMS, KPLEV  )
*   [INPUT] 
      REAL*8     GDZ  ( IJDIMS, KLEV   )
      REAL*8     GDPS ( IJDIMS )
      REAL*8     SIG  ( KLEV   )
      REAL*8     PLEV ( KPLEV  )
      REAL*8     RMISS
*
*   [WORK] 
      INTEGER    KDIMD, KPDIMD
      PARAMETER  (KDIMD=KMAX+1, KPDIMD=100)
      REAL*8     XP   ( KPDIMD )
      REAL*8     X    ( IJDIM, KDIMD )
      REAL*8     Y2   ( IJDIM, KDIMD )
      REAL*8     U    ( IJDIM, KDIMD )
      INTEGER    KU   ( IJDIM )
      REAL*8     S, P
      REAL*8     H, A, B 
      INTEGER    IJ, K, L, KZ, IFPAR, JFPAR
*
      REAL*8     BCRIT
      LOGICAL    OSPLIN
      LOGICAL    OFIRST
      NAMELIST  /NMS2PS/ BCRIT, OSPLIN
      DATA       BCRIT  / -1.0 /
      DATA       OSPLIN / .FALSE. /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         WRITE ( 6,* ) ' @@@ IS2PO: SIGtoP OUTPUT 94/07/07'
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMS2PS, END=190 )
  190    WRITE  ( JFPAR, NMS2PS )
      ENDIF
*
      DO 1100 K = 1, KPLEV
         XP( K ) = LOG( PLEV( K ) )
 1100 CONTINUE 
      DO 1200 K = 1, KLEV
         DO 1200 IJ = 1, IJDIMS
            X( IJ,K ) = LOG( GDPS( IJ )*SIG( K ) )
 1200 CONTINUE 
*
      IF ( OSPLIN ) THEN
         DO 2100 IJ = 1, IJDIMS
            Y2( IJ,1 ) = 0.
            U ( IJ,1 ) = 0.
 2100    CONTINUE 
         DO 2200 K = 2, KLEV-1
            DO 2200 IJ = 1, IJDIMS
               S        = (X(IJ,K)-X(IJ,K-1))/(X(IJ,K+1)-X(IJ,K-1))
               P        = S*Y2(IJ,K-1)+2.
               Y2(IJ,K) = (S-1.)/P
               U (IJ,K) = (6.*( ( GDZ(IJ,K+1) - GDZ(IJ,K) )
     &                         /( X  (IJ,K+1) - X  (IJ,K) )
     &                         -( GDZ(IJ,K) - GDZ(IJ,K-1) )
     &                         /( X  (IJ,K) - X  (IJ,K-1) ) )
     &                       /( X(IJ,K+1) - X(IJ,K-1)       )
     &                     - S*U(IJ,K-1)                       )/P
 2200    CONTINUE
         DO 2300 IJ = 1, IJDIMS
            Y2( IJ,KLEV ) = 0.
 2300    CONTINUE 
         DO 2400 K = KLEV-1, 1, -1
            DO 2400 IJ = 1, IJDIMS
               Y2( IJ,K ) = Y2(IJ,K)*Y2(IJ,K+1)+U(IJ,K)
 2400    CONTINUE
      ENDIF
*
      DO 3500 L = 1, KPLEV
         DO 3100 IJ = 1, IJDIMS
            KU( IJ ) = KLEV
 3100    CONTINUE 
         DO 3200 K = 1, KLEV-1
            DO 3200 IJ = 1, IJDIMS
               IF( X(IJ,K) .LT. XP(L) .AND. KU(IJ).EQ.KLEV ) THEN
                  KU( IJ ) = K
               ENDIF
 3200    CONTINUE 
*
         DO 3300 IJ = 1, IJDIMS
            KZ           = MAX( KU(IJ), 2 )
            H            = X(IJ,KZ) - X(IJ,KZ-1)
            A            = ( X(IJ,KZ) - XP(L)      )/H
            B            = ( XP(L)    - X(IJ,KZ-1) )/H
            IF ( RMISS .NE. 0. .AND. B .LT. BCRIT ) THEN
               GDZP( IJ,L ) = RMISS
            ELSE IF ( OSPLIN ) THEN
               GDZP( IJ,L ) = A*GDZ(IJ,KZ-1) + B*GDZ(IJ,KZ)
     &               + ( A*(A*A-1)*Y2(IJ,KZ-1) + B*(B*B-1)*Y2(IJ,KZ) )
     &                *(H*H)/6.
            ELSE
               GDZP( IJ,L ) = A*GDZ(IJ,KZ-1) + B*GDZ(IJ,KZ)
            ENDIF
 3300    CONTINUE 
 3500 CONTINUE 
*
      RETURN
*=======================================================================
      ENTRY      PSINTP    !! coordinate transformation p -> sigma
     O         ( GDZ   ,
     I           GDZP  , GDPS  , SIG   , 
     I           IJDIMS, KLEV  , 
     I           PLEV  , KPLEV           )
*
      DO 4100 K = 1, KPLEV
         XP( K ) = LOG( PLEV( K ) )
 4100 CONTINUE 
      DO 4200 K = 1, KLEV
         DO 4200 IJ = 1, IJDIMS
            X( IJ,K ) = LOG( GDPS( IJ )*SIG( K ) )
 4200 CONTINUE 
*
      DO 5500 K = 1, KLEV
         DO 5100 IJ = 1, IJDIMS
            KU( IJ ) = KPLEV+1
 5100    CONTINUE 
         DO 5200 L = 1, KPLEV
            DO 5200 IJ = 1, IJDIMS
               IF( X(IJ,K) .LT. XP(L) .AND. KU(IJ).GT.KPLEV ) THEN
                  KU( IJ ) = L
               ENDIF
 5200    CONTINUE 
*
         DO 5300 IJ = 1, IJDIMS
            IF ( KU( IJ ) .EQ. 1 ) THEN
               GDZ( IJ,K ) = GDZP(IJ,1)
            ELSE  IF ( KU( IJ ) .GT. KPLEV ) THEN
               GDZ( IJ,K ) = GDZP(IJ,KPLEV)
            ELSE
               KZ = KU( IJ )
               H  = XP(KZ) - XP(KZ-1)
               A  = ( XP(KZ)  - X(IJ,K)  )/H
               B  = ( X(IJ,K) - XP(KZ-1) )/H
               GDZP( IJ,K ) = A*GDZP(IJ,KZ-1) + B*GDZP(IJ,KZ)
            ENDIF
 5300    CONTINUE 
 5500 CONTINUE 
*
      RETURN
      END
