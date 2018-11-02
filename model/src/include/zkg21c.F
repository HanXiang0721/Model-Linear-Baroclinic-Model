* PACKAGE ZKG21C  !! surface vertical grid
*
*   [HIS] 95/06/21(numaguti) AGCM5.4f
*
#undef OPT_FROST
      INTEGER     KGMAX          !! No.of layers of ground (heat)
      INTEGER     KWMAX          !! No.of layers of ground (water)
*
      PARAMETER ( KGMAX  = 2,
     &            KWMAX  = 1   )
