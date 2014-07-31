\ TOOLS-EXT

( Conditional compilation as defined by )

: [ELSE] ( -- )
    1
    BEGIN
      BL WORD COUNT
      2DUP S" [IF]"   COMPARE 0= IF 2DROP 1+ ( increment nest count ) ELSE
      2DUP S" [ELSE]" COMPARE 0= IF 2DROP 1- DUP IF 1+ THEN ELSE
           S" [THEN]" COMPARE 0= IF 1- THEN THEN THEN
      ?DUP 0=
    UNTIL
; IMMEDIATE

: [IF]  ( flag -- )
    0= IF POSTPONE [ELSE] THEN ; IMMEDIATE

: [THEN]  ( -- ) ; IMMEDIATE
