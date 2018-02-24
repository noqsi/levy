module Levy.Lex (reserved, identifier, comma, semi, whiteSpace, natural
                , rational, brackets) where
import           Control.Applicative                  ((<|>))
import           Data.Monoid                          ((<>))
import           Text.Parsec                          (digit, letter, oneOf)
import qualified Text.Parsec.Language                 as Language
import           Text.Parsec.String                   (Parser)
import qualified Text.Parsec.Token                    as Token
import qualified Text.ParserCombinators.Parsec.Number as ParsecNumber

-- Various reserved names
units :: [String]
units = ["volt", "amp", "celsius", "kelvin", "adu", "counts"]

colors :: [String]
colors = ["green", "yellow", "red"]

countActionNames :: [String]
countActionNames = ["increment", "decrement", "reset"]

otherActionNames :: [String]
otherActionNames =
  [ "no_action"
  , "processor_reset"
  , "image_process_reset"
  , "fpe_reset"
  , "camera_off"
  , "camera_on"
  , "disable_recorder"
  , "enable_recorder"
  , "run_program"
  , "spawn_program"
  ]

opNames :: [String]
opNames = ["/", "*", "+", "-", "<", "<=", "=", ">=", ">"]

-- Lexer Declaration
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser $ Language.emptyDef
  { Token.commentLine     = "#" -- left over from cpp, throw away
  , Token.identStart      = letter
  , Token.identLetter     = letter <|> oneOf "_" <|> digit
  , Token.reservedOpNames = opNames
  , Token.reservedNames   =
     ["counter"] <> units <> colors <> countActionNames <> otherActionNames
  }

-- Parse reserved words, operators and identifiers
reserved :: String -> Parser ()
reserved = Token.reserved lexer

identifier :: Parser String
identifier = Token.identifier lexer

-- Parse comma , and semi-colon ; and skip trailing whitespace
comma :: Parser String
comma = Token.comma lexer

semi :: Parser String
semi = Token.semi lexer

-- Parse whitespace
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Parse numeric types
natural :: Parser Integer
natural = Token.natural lexer

rational :: Parser Rational
rational =
  do value <- ParsecNumber.fractional2 False
     _ <- whiteSpace
     return value

-- Parse Braces
brackets :: Parser p -> Parser p
brackets = Token.brackets lexer
