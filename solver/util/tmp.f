      PROGRAM tmp
      CHARACTER HEAD( 64 )*16
      OPEN(30, FILE='/disk18/ckj/models/ln_solver/bs/gt3/grz.t21', 
     & FORM='UNFORMATTED', STATUS='OLD' )
      READ(30) HEAD
      END
