\ STRING functions

: /STRING  ( caddr1 u1 n -- caddr2 u2 )                               \ 17.6.1.0245
    ROT OVER + ROT ROT - ;

: COMPARE  ( addr1 u1 addr2 u2 -- n )                                 \ 17.6.1.0935
    ?DUP
    IF
      OVER + SWAP
      DO
        ?DUP IF OVER C@ I C@ - ?DUP IF 0< 2* 1+ >R 2DROP R> UNLOOP EXIT THEN
             ELSE ( s1 shorter, but identical otherwise ) DROP -1 UNLOOP EXIT THEN
        1 /STRING
      LOOP
    ELSE
      2DROP  ( empty s2 )
    THEN
    ( identical, but s1 may be longer )
    SWAP DROP 0= 1+ ;
