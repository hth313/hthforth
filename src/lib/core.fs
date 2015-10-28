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
: DNEGATE  INVERT SWAP NEGATE SWAP OVER 0= - ;      \ word set DOUBLE
: *  UM* DROP ;
: S>D  DUP 0< ;
: ABS  S>D IF NEGATE THEN ;
: DABS  DUP 0< IF DNEGATE THEN ;                    \ word set DOUBLE

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
: PARSE  \ ( char "ccc<char>" -- c-addr u )                     \ word set CORE EXT
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
        1 >IN +!
        I C@ 41 = IF LEAVE THEN
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

: CR  ( -- )  10 EMIT ;

: FILL  ( c-addr u char -- )
    OVER
    IF >R OVER + SWAP R>
      DO DUP I C! LOOP
    ELSE 2DROP
    THEN
    DROP ;

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

( Compile normal words to compilee, immediates to user of POSTPONE )
: POSTPONE
    BL WORD FIND ?DUP
      IF 0< IF ( normal word, compile to compilee ) LIT, ['] COMPILE, THEN COMPILE,
      ELSE ABORT THEN
; IMMEDIATE

: S"  ( "ccc<quote> -- )   \ compile time
      ( -- c-addr u )      \ run-time
    34 PARSE STRING, ; IMMEDIATE

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

: [CHAR]  ( "<spaces>name" -- )    \ compile time
          ( -- char )              \ run-time
    BL DUP SKIP PARSE DROP C@ LIT, ; IMMEDIATE

: VARIABLE  ( "<spaces>name"-- )
    CREATE [ 1 CELLS ] LITERAL ALLOT ;

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

: SM/REM  ( d1 n1 -- n2 n3 )
    2DUP XOR >R   ( sign of quotient )
    OVER >R       ( sign of remainder )
    ABS >R DABS R>
    UM/MOD
    SWAP R> 0< IF NEGATE THEN
    SWAP R> 0< IF NEGATE THEN ;

: FM/MOD  ( d1 n1 -- n2 n3 )
    DUP R>
    SM/REM DUP 0< IF SWAP R> + SWAP 1+ ELSE R> DROP THEN ;

: /MOD  ( n1 n2 -- n3 n4 )
    >R S>D R> SM/REM ;

: MOD  ( n1 n2 -- n3 )
    /MOD DROP ;

: /  ( n1 n2 -- n3)
    /MOD SWAP DROP ;

: M*  ( n1 n2 -- d )
    2DUP XOR >R ABS SWAP ABS UM* R> 0< IF DNEGATE THEN ;

: */MOD  ( n1 n2 n3 - n4 n5 )
    >R M* R> SM/REM ;

: */  ( n1 n2 n3 -- n4 )
    */MOD SWAP DROP ;

: WITHIN  ( u ul uh -- f )                                \ word set CORE EXT
    OVER - >R - R> U< ;

: (NUMBER)
    DUP 48 57 WITHIN IF 48 -
    ELSE DUP 65 90 WITHIN IF 55 -
    ELSE DUP 97 122 WITHIN IF 87 - ELSE 0 EXIT THEN THEN THEN
    DUP BASE @ < ;

: >NUMBER  ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
    2DUP >R >R 0
    DO DUP C@ (NUMBER)
       IF SWAP 1+ >R >R SWAP BASE @ UM* ROT + SWAP R> + SWAP R> ELSE DROP LEAVE THEN
    LOOP
    DUP R> - R> SWAP - ;

: ,  ( n -- )
    HERE [ 1 CELLS ] LITERAL ALLOT ! ;

: C,  ( c -- )
    HERE [ 1 CHARS ] LITERAL ALLOT C! ;

: J  ( -- n )
    R> R> R@ SWAP >R SWAP >R ;

: UNLOOP  ( -- ) ( R: loop-sys -- )
    R> R> R> 2DROP >R ;

: ENVIRONMENT?  ( c-addr u -- false | i*x true )
    2DROP FALSE ;  ( we treat everything as unknown )
