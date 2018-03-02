module Main (main) where

import           Levy.AST
import           Levy.Parse
import           Test.Tasty         (defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Text.Parsec        (parse)
import           Text.Parsec.Error  (ParseError)
import           Text.Parsec.String (Parser)

runParser :: Parser t -> String -> Either ParseError t
runParser p = parse p ""

levyStart :: String
levyStart =
  "counter do_reset_1, reset_wait_1, reset_count_1a;          \
  \ counter do_power_off_1, power_off_wait_1;                 \
  \ counter do_power_on_1, power_on_wait_1, power_on_count_1; \

  \ counter operational_1;                                    \

  \ master_command = 18                                       \
  \ reset do_reset_1                                          \
  \ reset reset_wait_1                                        \
  \ reset reset_count_1                                       \
  \ reset do_power_off_1                                      \
  \ reset power_off_wait_1                                    \
  \ reset power_on_count_1                                    \
  \ increment do_power_on_1                                   \
  \ reset power_on_wait_1                                     \
  \ reset operational_1                                       \
  \ reset master_command                                      \
  \ ;"

main :: IO ()
main = defaultMain $ testGroup "LevyParser"
  [
    testCase "can parse counter declarations" $
      parseLevy "counter foo;" @?=
      Right (LevyProgram [CounterDeclaration ["foo"]])
  , testCase "Can parse a sensor id predicate term" $
      runParser predicateTermParser "123" @?=
      Right (SensorId 123)
  , testCase "can parse a counter predicate term" $
      runParser predicateTermParser "counter_123" @?=
      Right (CounterName "counter_123")
  , testCase "can parse a relation" $
      runParser relationParser "=" @?= Right Equals
  , testCase "can parse a relation" $
      runParser relationParser "=" @?= Right Equals
  , testCase "can parse a predicate with units" $
      runParser predicateParser "current = 10 amp" @?=
      Right (Predicate (CounterName "current") Equals (Const 10) (Just Amp))
  , testCase "can parse a predicate with units" $
      runParser predicateParser "current = 10" @?=
      Right (Predicate (CounterName "current") Equals (Const 10) Nothing)
  , testCase "can parse the beginning of the levy program" $
    parseLevy levyStart @?=
    Right
    (LevyProgram
     [ CounterDeclaration [ "do_reset_1"
                          , "reset_wait_1"
                          , "reset_count_1a" ]
     , CounterDeclaration [ "do_power_off_1"
                          , "power_off_wait_1" ]
     , CounterDeclaration [ "do_power_on_1"
                          , "power_on_wait_1"
                          , "power_on_count_1" ]
     , CounterDeclaration [ "operational_1" ]
     , Command
         (Predicate
           (CounterName "master_command")
           Equals
           (Const 18)
           Nothing)
         [ CounterAction Nothing Reset "do_reset_1"
         , CounterAction Nothing Reset "reset_wait_1"
         , CounterAction Nothing Reset "reset_count_1"
         , CounterAction Nothing Reset "do_power_off_1"
         , CounterAction Nothing Reset "power_off_wait_1"
         , CounterAction Nothing Reset "power_on_count_1"
         , CounterAction Nothing Increment "do_power_on_1"
         , CounterAction Nothing Reset "power_on_wait_1"
         , CounterAction Nothing Reset "operational_1"
         , CounterAction Nothing Reset "master_command" ]])
  ]
