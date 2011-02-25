{-# LANGUAGE  RankNTypes #-}
{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth parser.

  This is a simplified Forth parser inteded to parse the Core set of variables and
  colon definitions. It will not handle everything a real Forth parser will allow
  you to and does not behave strictly as Forth does.

  When the Core set has been read, it is meant to be used to process further
  Forth code, which will allow the full blown rich set of Forth behavior for all
  extension sets.

-}

module Forth.Parser (parseForth) where

import Forth.Cell
import Forth.Machine
import Forth.Types
import Text.Parsec.Prim
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Control.Monad.Trans

type Parser a = Cell cell => ParsecT String () (MachineM cell) a

parseErrorText err =
    let text = tail $ snd $ break ('\n'==) (show err)
        join [] = []
        join [s] = s
        join (l:ls) = l ++ ", " ++ join ls
    in join (lines text)

-- | The parser is based on Parsec and run as a Monad tranformer with the Forth Machine
--   monad as the inner monad.
parseForth :: Cell cell => String -> String -> (MachineM cell) (Either String ())
parseForth screenName text = do
    result <- runParserT (whiteSpace >> topLevel) () screenName text
    return $ case result of
               Left err -> Left (parseErrorText err)
               Right val -> Right val

lexer :: Cell cell => P.GenTokenParser String () (MachineM cell)
lexer  = P.makeTokenParser (P.LanguageDef { P.commentStart = "( ",
                                            P.commentEnd = ")",
                                            P.commentLine = "\\",
                                            P.nestedComments = False,
                                            P.reservedNames = [":", ";", "CREATE",
                                                               "VARIABLE", "CONSTANT",
                                                               "IF", "ELSE", "THEN",
                                                               "BEGIN", "WHILE", "UNTIL",
                                                               "REPEAT", "UNTIL"
                                                              ],
                                            P.identStart = wordChar,
                                            P.identLetter = wordChar,
                                            P.opStart = never,
                                            P.opLetter = never,
                                            P.reservedOpNames = [],
                                            P.caseSensitive = True } )
identifier :: Parser String
identifier = P.identifier lexer

reserved :: String -> Parser ()
reserved name = P.reserved lexer name

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

-- Most characters are accepted in a Forth word, but it can be a bit more restrictive
-- than this
wordChar, never :: Parser Char
wordChar = noneOf " \t\n"
never = satisfy (const False)

-- Parse a top level construct
topLevel :: Parser ()
topLevel = many definition >> eof

definition :: Parser ()
definition = colonDef <|> create <|> variable <|> constant <|> exec

colonDef :: Parser ()
colonDef = do
  reserved ":"
  name <- identifier
  lift $ executeSlice [ "-1", "STATE", "!" ]
  lift $ openColonDef name
  body <- manyTill colonWord (reserved ";")
  lift $ closeColonDef
  lift $ executeSlice [ "0", "STATE", "!" ]
  return ()

colonWord :: Parser ()
colonWord = compileToken <|>
            (construct "IF" IF) <|>
            (construct "ELSE" ELSE) <|>
            (construct "THEN" THEN) <|>
            (construct "BEGIN" BEGIN) <|>
            (construct "WHILE" WHILE) <|>
            (construct "UNTIL" UNTIL) <|>
            (construct "REPEAT" REPEAT) <?> "word"

construct name cons = (reserved name >> lift (compileWord (Structure cons)))

compileToken :: Parser ()
compileToken = do
  name <- identifier
  result <- lift $ perform name
  case result of
    True -> return ()
    False -> unexpected name

create :: Parser ()
create = do
  reserved "CREATE"
  name <- identifier
  return ()

variable :: Parser ()
variable = do
  reserved "VARIABLE"
  name <- identifier
  lift $ createVariable name

constant :: Parser ()
constant = do
  reserved "CONSTANT"
  name <- identifier
  lift $ createConstant name

exec :: Parser ()
exec = do
  name <- identifier
  word <- lift $ wordFromName name
  case word of
    Just (WordRef key) -> lift $ execute key
    Just (Literal lit) -> lift $ pushLiteral lit
    Nothing -> unexpected name