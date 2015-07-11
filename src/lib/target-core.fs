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
