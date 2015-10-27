\ Forth core words for a target.

\ A target starts out with only a few primitives that are expected to be
\ implemented in native code. This file fills up the core dictionary
\ for a target.

\ Stack primitives
: ROT    >R SWAP R> SWAP ;
: 2DROP  DROP DROP ;
: 2DUP   OVER OVER ;
: 2SWAP  ROT >R ROT R> ;
: 2OVER  >R >R 2DUP R> R> 2SWAP ;

\ Arithmetic and logical
: 1+  1 + ;
: 1-  1 - ;
\ : INVERT  TRUE XOR ;
\ : NEGATE  INVERT 1+ ;
\ : DNEGATE  INVERT SWAP NEGATE SWAP OVER 0= - ;      \ word set DOUBLE
: *  UM* DROP ;
: S>D  DUP 0< ;
\ : ABS  S>D IF NEGATE THEN ;
\ : DABS  DUP 0< IF DNEGATE THEN ;                    \ word set DOUBLE

\ Comparison
: =  - 0= ;
: <  - 0< ;
: >  SWAP < ;
: U<  2DUP XOR 0< IF SWAP DROP 0< EXIT THEN - 0< ;

\ Loops
: (DO)
  R> ROT >R SWAP >R >R ;
