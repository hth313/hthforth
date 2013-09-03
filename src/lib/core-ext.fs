( Core extensions )

0    CONSTANT FALSE
0 0= CONSTANT TRUE

: \
    1 PARSE-DO
      I @ 10 = IF LEAVE THEN 1+
    LOOP
    >IN +! ; IMMEDIATE


: NIP  ( x1 x2 -- x2 )
    SWAP DROP ;

: TUCK  ( x1 x2 -- x2 x1 x2 )
    SWAP OVER ;

\ PARSE reside among the core words in this implementation as it is used
\ as a building block for WORD.


\ Skip leading space delimiters. Parse name delimited by a space
: PARSE-NAME  ( "<spaces>name<space> -- caddr u )
    BL DUP SKIP PARSE ;

: WITHIN  ( u ul uh -- f )
    OVER - >R - R> U< ;
