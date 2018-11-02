program test
implicit none
!character head(64)*16
      REAL*8      FACT( 11 )         !! factor
      CHARACTER*90 CFS          !! input file name (gtool3)
      CHARACTER*90 CFC          !! input file name (gtool3)
      CHARACTER*90 CFU          !! input file name (gtool3)
      CHARACTER*90 CFV          !! input file name (gtool3)
      CHARACTER*90 CFW          !! input file name (gtool3)
      CHARACTER*90 CFT          !! input file name (gtool3)
      CHARACTER*90 CFZ          !! input file name (gtool3)
      CHARACTER*90 CFP          !! input file name (gtool3)
      CHARACTER*90 CFQ          !! input file name (gtool3)
      CHARACTER*90 CFTC         !! input file name (gtool3)
      CHARACTER*90 CFQC         !! input file name (gtool3)
      CHARACTER*90 CFTL         !! input file name (gtool3)
      CHARACTER*90 CFQL         !! input file name (gtool3)
      CHARACTER*90 CFPR         !! input file name (gtool3)
      CHARACTER*90 CFO          !! output file name (grads)
      CHARACTER*90 CBS0         !! basic state (dummy)
      CHARACTER*90 CBS          !! basic state
      LOGICAL      OPL          !! convert to p-level?
      LOGICAL      OCLASSIC     !! conventional?
      NAMELIST    /NMFGT/   CFS, CFC, CFU, CFV, CFW, CFT, CFZ, CFP, CFQ, CFTC, CFQC, CFO, FACT, OPL 
      NAMELIST    /NMBS/    CBS0, CBS 
      NAMELIST    /NMCLS/   OCLASSIC
      DATA         FACT      / 11*1.D0 /
      DATA         OPL       / .TRUE. /
      DATA         OCLASSIC  / .TRUE. /
!OPEN (30, FILE='/disk18/ckj/models/ln_solver/bs/gt3/grz.t21', FORM='UNFORMATTED', STATUS='OLD' )
!OPEN (30, FILE='grz.head', FORM='UNFORMATTED', STATUS='OLD' )

 !        write(nout, *) CALT
 !         write(6,*)  'aaa'
 !         READ( 30 ) head
 !         write(6, *)   ' finish reading head: ',head
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')

!      READ( 1, NMFGT)

end program test
