( block 100 )

: NIP ( n1 n2 -- n2)   SWAP DROP ;
: 2DROP   DROP DROP ;
: 2DUP  OVER OVER ;
: ?DUP  DUP IF DUP THEN ;

: 1+  1 + ;
: 1-  1 - ;
: INVERT  -1 XOR ;
: NEGATE  INVERT 1+ ;
: ABS  DUP 0< IF NEGATE THEN ;
( shadow 100 )
( block 101 )
( shadow 101 )
( block 102 )

( shadow 102 )
( block 103 )
( shadow 103 )
( block 104 )
( shadow 104 )
( block 105 interpreter )

CREATE _INPUT-BUFFER 80 CHARS ALLOT ( may do this internally? )

: EVALUATE  >IN @ >R 0 >IN ! SOURCE >R >R #IN 2! _INTERPRET
    R> R> #IN 2! R> >IN ! ;

: QUIT  _RESET-RSTACK
      BEGIN
        BEGIN
	  _READ-LINE 0 >IN ! _INPUT-BUFFER 0 EVALUATE CR
	  STATE @
        UNTIL ." ok "  ( exhausted input in interpretation mode )
      AGAIN ;

  ( shadow 105 )
( block 106 )
( shadow 106 )
( block 107 )
( shadow 107 )
( block 108 )
( shadow 108 )
( block 109 )
( shadow 109 )
( block 110 compiler )

: VARIABLE CREATE 1 CELLS ALLOT ;

VARIABLE STATE  ( compilation state variable )
0 STATE !       ( interpreting by default )
: [  FALSE STATE ! ;
: ]  TRUE STATE ! ;

( Colon definitions )
: :  CREATE ] ;
: ;  POSTPONE EXIT SMUDGE [ ; IMMEDIATE

( shadow 110 )
( block 111 )
( shadow 111 )
( block 112 )
( shadow 112 )
