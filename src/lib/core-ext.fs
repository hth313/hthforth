( Core extensions )

\ PARSE reside among the core words in this implementation as it is used
\ as a building block for WORD.


\ Skip leading space delimiters. Parse name delimited by a space
: PARSE-NAME  ( "<spaces>name<space> -- caddr u )
    BL DUP SKIP PARSE ;
