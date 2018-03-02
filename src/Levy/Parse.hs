module Levy.Parse (parseLevy, predicateTermParser
                  , relationParser, predicateParser) where
import           Control.Applicative    (many, (<|>))
import           Data.Foldable          (asum)
import           Levy.AST
import           Levy.Lex
import           Text.Parsec            (ParseError, parse, sepBy1)
import           Text.Parsec.Combinator (many1, optionMaybe)
import           Text.Parsec.String     (Parser)

counterDeclarationParser :: Parser Statement
counterDeclarationParser =
  do _            <- reserved "counter"
     counterNames <- sepBy1 identifier comma
     _            <- semi
     return (CounterDeclaration counterNames)

predicateTermParser :: Parser PredicateTerm
predicateTermParser = (SensorId <$> natural) <|> (CounterName <$> identifier)

unitParser :: Parser Unit
unitParser =
  asum [ reserved "volt"    >> return Volt
       , reserved "amp"     >> return Amp
       , reserved "celsius" >> return Celsius
       , reserved "kelvin"  >> return Kelvin
       , reserved "adu"     >> return ADU
       , reserved "count"   >> return Counts ]

arithParser :: Parser Arith
arithParser =
  asum [Const <$> rational] -- TODO: Negation, BinOps

relationParser :: Parser Relation
relationParser =
  asum [ reserved "="  >> return Equals
       , reserved ">"  >> return GreaterThan
       , reserved "<"  >> return LessThan
       , reserved ">=" >> return GreaterThanOrEquals
       , reserved "<=" >> return LessThanOrEquals ]

predicateParser :: Parser Predicate
predicateParser =
  do
    predicateTerm <- predicateTermParser
    relation      <- relationParser
    arith         <- arithParser
    maybeUnit     <- optionMaybe unitParser
    return (Predicate predicateTerm relation arith maybeUnit)

colorParser :: Parser Color
colorParser =
  asum [ reserved "green"  >> return Green
       , reserved "yellow" >> return Yellow
       , reserved "red"    >> return Red ]

counterActionItemParser :: Parser CounterActionItem
counterActionItemParser =
  asum [ reserved "increment" >> return Increment
       , reserved "decrement" >> return Decrement
       , reserved "reset"     >> return Reset ]

counterActionParser :: Parser Action
counterActionParser =
  do
    maybeColor        <- optionMaybe colorParser
    counterActionItem <- counterActionItemParser
    counterName       <- identifier
    return (CounterAction maybeColor counterActionItem counterName)

otherActionItemParser :: Parser OtherActionItem
otherActionItemParser =
  asum [ reserved "no_action"           >> return NoAction
       , reserved "processor_reset"     >> return ProcessorReset
       , reserved "image_process_reset" >> return ImageProcessReset
       , reserved "fpe_reset"           >> return FpeReset
       , reserved "camera_off"          >> return CameraOff
       , reserved "camera_on"           >> return CameraOn
       , reserved "disable_recorder"    >> return DisableRecorder
       , reserved "enable_recorder"     >> return EnableRecorder
       , reserved "run_program"         >> return RunProgram
       , reserved "spawn_program"       >> return SpawnProgram ]

otherActionParser :: Parser Action
otherActionParser =
  do
    otherActionItem <- otherActionItemParser
    number          <- natural
    return (OtherAction otherActionItem number)

actionParser :: Parser Action
actionParser = counterActionParser <|> otherActionParser

commandParser :: Parser Statement
commandParser =
  do predicate    <- predicateParser
     commands     <- many1 actionParser
     _            <- semi
     return (Command predicate commands)

statementParser :: Parser Statement
statementParser = counterDeclarationParser <|> commandParser

levyProgramParser :: Parser LevyProgram
levyProgramParser = whiteSpace >> LevyProgram <$> many statementParser

parseLevy :: String -> Either ParseError LevyProgram
parseLevy = parse levyProgramParser ""
