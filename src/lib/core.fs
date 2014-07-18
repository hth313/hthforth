\ Forth core words

\ When this file is loaded, only a very small subset of words are available.
\ They are defined in the Haskell module Language.Forth.Core
\ Only backslash comments are available at this point, we will define ( comments
\ further down in this file.
\ The order of the words are dictated by the fact that we do not have so many
\ words available when we start.


\ Compile state
: [  FALSE STATE ! ; IMMEDIATE
: ]  TRUE STATE ! ;

: SOURCE  INPUT-LINE @ #INPUT-LINE @ ;

\ Stack primitives
: ROT    >R SWAP R> SWAP ;
: ?DUP   DUP IF DUP THEN ;
: 2DROP  DROP DROP ;
: 2DUP   OVER OVER ;
: 2SWAP  ROT >R ROT R> ;
: 2OVER  >R >R 2DUP R> R> 2SWAP ;

\ Arithmetic and logical
: 1+  1 + ;
: 1-  1 - ;
: INVERT  TRUE XOR ;
: NEGATE  INVERT 1+ ;
: *  UM* DROP ;
: S>D  DUP 0< ;
: ABS  S>D IF NEGATE THEN ;

: +!  \  ( n a-addr -- )
    DUP @ ROT + SWAP ! ;

\ Comparison
: =  - 0= ;
: <  - 0< ;
: >  SWAP < ;
: U<  2DUP XOR 0< IF SWAP DROP 0< EXIT THEN - 0< ;

: PARSE-POS  \ ( -- caddr )
    SOURCE DROP >IN @ + ;

32 CONSTANT BL

\ Parse delimiter test. BL delimiters will match all character values
\ up to BL. Other delimiters are precise.
\ This is a tad inefficient as we test the delimiter for each chararcter
\ we inspect (currying and higher order functions would be nice).
\ On the other hand, parsing is not what we do most, and especially
\ not what will be time or power critical.
: ?DELIM  \ ( char delimiter -- flag )
    DUP BL = IF 1+ < ELSE = THEN ;

\ Setup loop for current parse buffer, flag tells whether
\ we are fit to loop (false means skip loop)
: SRC-SPAN  \ ( -- limit start flag )
    SOURCE + PARSE-POS 2DUP - ;

\ Parse ccc delimited by delimiter char.
\ Parse is part of the core words here as it is used as a building
\ block for WORD.
\ This word is part of core ext, but it is very nice for parsing
\ and used here anyway.
: PARSE  \ ( char "ccc<char>" -- c-addr u )                     ( core ext )
    PARSE-POS 0 ROT SWAP
    SRC-SPAN IF
      DO
        OVER I C@ SWAP ?DELIM
        IF 1 >IN +! LEAVE ELSE 1+ THEN
      LOOP
    THEN
    SWAP DROP DUP >IN +! ;

\ Comments
: (
    SRC-SPAN IF
      DO
        I C@ 41 = IF LEAVE THEN
        1 >IN +!
      LOOP
    THEN
; IMMEDIATE

\ Skip delimiter characters from the input stream.
: SKIP  ( char "<chars>" -- )
    SRC-SPAN IF
      DO
        I C@ OVER ?DELIM 0= IF LEAVE ELSE 1 >IN +! THEN
      LOOP
    THEN
    DROP ( delimiter )
    ;

: TYPE  ( caddr u -- )
    OVER + SWAP
    DO I C@ EMIT LOOP ;

: SPACE  ( -- )
    BL EMIT ;

: SPACES  ( n -- )
    DUP 0 > IF 0 DO SPACE LOOP ELSE DROP THEN ;

\ If flag is set, ABORT
\ This is useful as a way out of strange unexpected problems when we
\ do not even bother to tell the user what was wrong.
: ?ABORT  ( flag -- )
    IF ABORT THEN ;

\ Copy string to transient region (TREG), max 31 characters
: >TREG  ( caddr u -- ccaddr )
    31 OVER U< ?ABORT
    TREG >R R@ 2DUP C!
    1+ SWAP MOVE R> ;

: WORD  ( char "<chars>name<char> -- counted-c-addr )
    DUP SKIP PARSE >TREG ;

: '    ( "<spaces>name" -- xt )
    BL WORD FIND IF STATE @ IF LIT, THEN ELSE ABORT THEN ;

: [']  ( "<spaces>name" -- xt )
    '  ; IMMEDIATE

: LITERAL   LIT, ; IMMEDIATE

: POSTPONE
    BL WORD FIND
    IF LIT, ['] COMPILE, COMPILE, ELSE ABORT THEN ; IMMEDIATE

: ABORT"
    34 PARSE STRING, POSTPONE TYPE POSTPONE ABORT ; IMMEDIATE

: ."  ( "ccc<quote> -- )
    34 PARSE STRING, POSTPONE TYPE ; IMMEDIATE

( CELL related. These need adjustment depending on cell size. )
( For now we assume 32-bits. )
: CELLS  ( n1 -- n2 )
    2* 2* ;
: CELL+  ( a-addr1 -- a-addr2 )
    4 + ;

( Character related, here we assume a character occupy a single )
( address unit )
: CHARS  ( n1 -- n2 )  ;
: CHAR+  ( c-addr1 -- c-addr2 )  1+ ;
: CHAR  ( "<spaces>name" -- char )
    BL WORD DUP C@ IF CHAR+ C@ THEN ;

: VARIABLE  ( "<spaces>name"-- )
    CREATE 1 CELLS ALLOT ;

( Numeric base )
VARIABLE BASE
: DECIMAL 10 BASE ! ;   : HEX 16 BASE ! ;
DECIMAL

: 2!   ( x1 x2 a-addr -- )
    SWAP OVER ! CELL+ ! ;
: 2@   ( a-addr -- x1 x2 )
    DUP CELL+ @ SWAP @ ;
: COUNT   ( c-addr1 -- c-addr2 u )
    DUP CHAR+ SWAP C@ ;

( Numeric output primitives )
VARIABLE HLD

: <#  ( -- )
    PAD HLD ! ;
: #>  ( xd -- c-addr u )
    2DROP HLD @ PAD OVER - ;
: HOLD  ( c -- )
    HLD @ 1 CHARS - DUP HLD ! C! ;
: #  ( ud1 -- ud2 )
    0 BASE @ UM/MOD >R BASE @ UM/MOD SWAP 9 OVER < 7 AND + 48 + HOLD R> ;
: #S  ( ud1 -- ud2 )
    BEGIN # 2DUP OR 0= UNTIL ;
: SIGN  ( n -- )
    0< IF 45 ( - ) HOLD THEN ;

: U.  ( u -- )
    0 <# #S #> TYPE SPACE ;

: .  ( n -- )
    DUP ABS 0 <# #S ROT SIGN #> TYPE SPACE ;


@@
: ?PAIRS  - IF ABORT" unmatched conditionals" THEN ;

\ Conditionals
: IF    HERE POSTPONE JUMP-FALSE 0 COMPILE, 2 ; IMMEDIATE
: ELSE  2 ?PAIRS HERE POSTPONE JUMP 0 COMPILE, HERE ROT BACKPATCH 2 ; IMMEDIATE
: THEN  2 ?PAIRS HERE SWAP BACKPATCH ; IMMEDIATE

\ Loops
: BACK     HERE COMPILE, ;  ( backward branch )
: DO      POSTPONE (DO) HERE 3 ; IMMEDIATE
: LOOP    3 ?PAIRS POSTPONE (LOOP)  POSTPONE JUMP-FALSE BACK ; IMMEDIATE
: +LOOP   3 ?PAIRS COMPILE, POSTPONE (+LOOP) BACK ; IMMEDIATE


: MAX  ( n n -- n )
    2DUP < IF SWAP THEN DROP ;
: MIN ( n n -- n)
    2DUP > IF SWAP THEN DROP ;






VARIABLE BLK
: FH
    BLK @ + ;  \ relative block
: LOAD
    BLK @ SWAP DUP BLK ! (LOAD) BLK ! ;

( Not part of CORE, disabled at the moment )
\ : -ROT  SWAP >R SWAP R> ;  \ or ROT ROT
\ : NIP  ( n1 n2 -- n2 )       SWAP DROP ;
\ : TUCK ( n1 n2 -- n2 n1 n2 ) SWAP OVER ;

\ Comparisons
-1 CONSTANT TRUE   0 CONSTANT FALSE

:  =  ( n n -- f)
    XOR  0= ;
:  < ( n n -- f )
    - 0< ;
:  > ( n n -- f )
    SWAP < ;

: WITHIN  ( u ul uh -- f )
    OVER - >R - R> U< ;

\ Arithmetic and logical
: 1+  1 + ;
: 1-  1 - ;
: INVERT  TRUE XOR ;
: NEGATE  INVERT 1+ ;
: DNEGATE  INVERT SWAP NEGATE SWAP OVER 0= - ;
: S>D  ( n -- d ) DUP 0< ;   \ sign extend
: ABS  S>D IF NEGATE THEN ;
: DABS  DUP 0< IF DNEGATE THEN ;

: +-
    0< IF NEGATE THEN ;
: D+-
    0< IF DNEGATE THEN ;

\ Variables
VARIABLE BASE
: DECIMAL 10 BASE ! ;   : HEX 16 BASE ! ;

VARIABLE DP

\ Math
: SM/REM ( d n -- r q )  \ symmetric
    OVER >R >R DABS R@ ABS UM/MOD
    R> R@ XOR 0< IF NEGATE THEN
    R> 0< IF >R NEGATE R> THEN ;

: FM/MOD ( d n -- r q )  \ floored
    DUP 0< DUP >R IF NEGATE >R DNEGATE R> THEN
    >R DUP 0< IF R@ + THEN
    R> UM/MOD R> IF >R NEGATE R> THEN ;

: /MOD  OVER 0< SWAP FM/MOD ;
: MOD   /MOD DROP ;
: /     /MOD SWAP DROP ;

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

\ Numeric output primitives
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

\ Memory access
: +! ( n a-addr -- )  DUP >R @ + R> ! ;
: 2! ( x1 x2 a-addr -- )  SWAP OVER ! CELL+ ! ;
: 2@ ( a-addr -- x1 x2 )  DUP CELL+ @ SWAP @ ;
: COUNT ( c-addr1 -- c-addr2 u )  DUP CHAR+ SWAP C@ ;

\ Compiler
: [  FALSE STATE ! ; IMMEDIATE
: ]  TRUE STATE ! ;

: ALLOT ( n -- )  DP +! ;
: HERE ( -- a ) DP @ ;
: , ( n -- )  HERE [ 1 CELLS ] LITERAL ALLOT ! ;
: COMPILE, ( xt -- )  HERE [ 1 INSTRS ] LITERAL ALLOT ! ;
: LITERAL ( x -- )  ['] _LIT COMPILE, , ; IMMEDIATE

: VARIABLE CREATE 1 CELLS ALLOT ;
: CONSTANT CREATE , DOES> @ ;

( Colon definitions )
: :  CREATE ] ;
: ;  POSTPONE EXIT SMUDGE [ ; IMMEDIATE

\ Interpreter
CREATE _INPUT-BUFFER 80 CHARS ALLOT ( may do this internally? )

: EVALUATE
    >IN @ >R 0 >IN ! SOURCE >R >R #IN 2! _INTERPRET
    R> R> #IN 2! R> >IN ! ;

: QUIT  _RESET-RSTACK
    BEGIN
	BEGIN
	    _READ-LINE 0 >IN ! _INPUT-BUFFER 0 EVALUATE CR
	    STATE @
	UNTIL ." ok "  ( exhausted input in interpretation mode )
    AGAIN ;
