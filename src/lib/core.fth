( block 1  -- Main load block )

VARIABLE STATE  ( compilation state variable )
0 STATE !       ( interpreting by default )

VARIABLE BLK
: FH  BLK @ + ;  \ relative block
: LOAD  BLK @ SWAP DUP BLK ! (LOAD) BLK ! ;

30 LOAD
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

( block 30  CORE words )

1 FH 8 FH THRU

( shadow 30 )
( block 31 stack primitives )

: ROT   >R SWAP R> SWAP ;
: ?DUP  DUP IF DUP THEN ;

( Not part of CORE, disabled at the moment )
\ : -ROT  SWAP >R SWAP R> ;  \ or ROT ROT
\ : NIP  ( n1 n2 -- n2 )       SWAP DROP ;
\ : TUCK ( n1 n2 -- n2 n1 n2 ) SWAP OVER ;

: 2DROP   DROP DROP ;
: 2DUP  OVER OVER ;
: 2SWAP  ROT >R ROT R> ;
: 2OVER  >R >R 2DUP R> R> 2SWAP ;
( shadow 31 )
( block 32  comparisons )

-1 CONSTANT TRUE   0 CONSTANT FALSE

:  =  ( n n -- f) XOR  0= ;
:  < ( n n -- f ) - 0< ;
:  > ( n n -- f ) SWAP < ;

: MAX ( n n -- n ) 2DUP < IF SWAP THEN DROP ;
: MIN ( n n -- n ) 2DUP > IF SWAP THEN DROP ;

: WITHIN  ( u ul uh -- f ) OVER - >R - R> U< ;
( shadow 32 )
( block 33 ALU )

: 1+  1 + ;
: 1-  1 - ;
: INVERT  TRUE XOR ;
: NEGATE  INVERT 1+ ;
: DNEGATE  INVERT SWAP NEGATE SWAP OVER 0= - ;
: S>D  ( n -- d ) DUP 0< ;   \ sign extend
: ABS  S>D IF NEGATE THEN ;
: DABS  DUP 0< IF DNEGATE THEN ;

: +-  0< IF NEGATE THEN ;
: D+- 0< IF DNEGATE THEN ;

( shadow 33 )
( block 34  variables )

VARIABLE BASE
: DECIMAL 10 BASE ! ;   : HEX 16 BASE ! ;

VARIABLE DP
( shadow 34 )
( block 35 math )

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
: /    /MOD SWAP DROP ;

( shadow 35 )
( block 36  math continued )

: *  UM* DROP ;
: M*  2DUP XOR R> ABS SWAP ABS UM* R> D+- ;
: */MOD  >R M* R> FM/MOD ;
: */     */MOD SWAP DROP ;

: 2* DUP + ;
\ 2/ which is right shift is native

: LSHIFT ( x1 u -- x2 )
  BEGIN DUP WHILE SWAP 2* SWAP 1- REPEAT DROP ;

: RSHIFT ( x1 u -- x2 )
  BEGIN DUP WHILE SWAP 2/ SWAP 1- REPEAT DROP ;
  ( shadow 36 )
( block 37 numeric output primitives )

VARIABLE HLD
: HERE ( -- addr )  DP @ ;
: PAD ( -- c-addr )  HERE 64 CHARS + ;

: <# ( -- )  PAD HLD ! ;
: #> ( xd -- c-addr u )  2DROP HLD @ PAD OVER - ;
: HOLD ( c -- )  HLD @ -1 CHARS - DUP HLD ! C! ;
: DIGIT ( u -- c )  9 OVER < 7 AND + 30 +  ;
: # ( ud1 -- ud2 )
  0 BASE @ UM/MOD >R BASE @ UM/MOD SWAP DIGIT HOLD R> ;
: #S ( ud1 -- ud2 )  BEGIN # 2DUP OR 0= UNTIL ;
: SIGN ( n -- )  0< IF 45 ( - ) HOLD THEN ;

( shadow 37 )
( block 38 memory access )

: +! ( n a-addr -- )  DUP >R @ + R> ! ;
: 2! ( x1 x2 a-addr -- )  SWAP OVER ! CELL+ ! ;
: 2@ ( a-addr -- x1 x2 )  DUP CELL+ @ SWAP @ ;
: COUNT ( c-addr1 -- c-addr2 u )  DUP CHAR+ SWAP C@ ;
( shadow 38 )
( block 39 )
( shadow 39 )
( block 40 compiler )

: [  FALSE STATE ! ; IMMEDIATE
: ]  TRUE STATE ! ;

: LITERAL ( x -- )  ['] (LIT) , ; IMMEDIATE
: ALLOT ( n -- )  DP +! ;

: COMPILE, ( xt -- )  HERE [ 1 XTS ] LITERAL ALLOT ! ;

( Data allocation )
: , ( n -- )  HERE [ 1 CELLS ] LITERAL ALLOT ! ;

: VARIABLE CREATE 1 CELLS ALLOT ;
: CONSTANT CREATE , DOES> @ ;

( Colon definitions )
: :  CREATE ] ;
: ;  POSTPONE EXIT SMUDGE [ ; IMMEDIATE

( shadow 40 )
( block 41 )
( shadow 41 )
( block 42 interpreter )

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
( shadow 42 )
