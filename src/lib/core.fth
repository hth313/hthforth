( block 1  -- Main load block )

VARIABLE BLK
: FH  BLK @ + ;  \ relative block
: LOAD  BLK @ SWAP DUP BLK ! (LOAD) BLK ! ;

100 LOAD
( shadow 1 )
( block 2 )
( shadow 2 )
( block 3 )
( shadow 3 )

( block 10 )
( shadow 10 )
( block 11 )
( shadow 11 )
( block 12 )
( shadow 12 )

( block 100 )

1 FH 6 FH THRU

( shadow 100 )
( block 101 stack primitives )

: ROT   >R SWAP R> SWAP ;
: -ROT  SWAP >R SWAP R> ;  \ or ROT ROT
: ?DUP  DUP IF DUP THEN ;
: NIP  ( n1 n2 -- n2 )       SWAP DROP ;
: TUCK ( n1 n2 -- n2 n1 n2 ) SWAP OVER ;

: 2DROP   DROP DROP ;
: 2DUP  OVER OVER ;
: 2SWAP  ROT >R ROT R> ;
: 2OVER  >R >R 2DUP R> R> 2SWAP ;
( shadow 101 )
( block 102  comparisons )

-1 CONSTANT TRUE   0 CONSTANT FALSE

:  =  ( n n -- f) XOR  0= ;
:  < ( n n -- f ) - 0< ;
:  > ( n n -- f ) SWAP < ;

: MAX ( n n -- n ) 2DUP < IF SWAP THEN DROP ;
: MIN ( n n -- n ) 2DUP > IF SWAP THEN DROP ;

: WITHIN  ( u ul uh -- f ) OVER - >R - R> U< ;
( shadow 102 )
( block 103 ALU )

: 1+  1 + ;
: 1-  1 - ;
: INVERT  TRUE XOR ;
: NEGATE  INVERT 1+ ;
: DNEGATE  INVERT SWAP NEGATE TUCK 0= - ;
: S>D  ( n -- d ) DUP 0< ;   \ sign extend
: ABS  S>D IF NEGATE THEN ;
: DABS  DUP 0< IF DNEGATE THEN ;


: +-  0< IF NEGATE THEN ;
: D+- 0< IF DNEGATE THEN ;

( shadow 103 )
( block 104  variables )

VARIABLE BASE
: DECIMAL 10 BASE ! ;   : HEX 16 BASE ! ;
( shadow 104 )
( block 105 math )

: SM/REM ( d n -- r q )  \ symmetric
  OVER >R >R DABS R@ ABS UM/MOD
  R> R@ XOR 0< IF NEGATE THEN
  R> 0< IF >R NEGATE R> THEN ;

: FM/MOD ( d n -- r q )  \ floored
  DUP 0< DUP >R IF NEGATE >R DNEGATE R> THEN
  >R DUP 0< IF R@ + THEN
  R> UM/MOD R> IF >R NEGATE R> THEN ;

: /MOD OVER 0< SWAP FM/MOD ;
: MOD  /MOD DROP ;
: /    /MOD NIP ;

( shadow 105 )
( block 106  math continued )

: *  UM* DROP ;
: M*  2DUP XOR R> ABS SWAP ABS UM* R> D+- ;
: */MOD  >R M* R> FM/MOD ;
: */     */MOD NIP ;

: 2* DUP + ;
\ 2/ which is right shift is native
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
( block 112 interpreter )

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
( shadow 112 )
