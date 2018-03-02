module Levy.AST where

newtype LevyProgram = LevyProgram [Statement] deriving (Eq, Show)
type CounterName = String
data Statement = CounterDeclaration [CounterName]
               | Command Predicate [Action] deriving (Eq, Show)
data PredicateTerm = SensorId Integer
                   | CounterName String deriving (Eq, Show)
data Unit = Volt
          | Amp
          | Celsius
          | Kelvin
          | ADU
          | Counts deriving (Eq, Show)
data ArithOp = Add | Subtract | Multiply | Divide deriving (Eq, Show)
data Arith = Const Rational
           | Neg Arith
           | ArithBinary ArithOp Arith Arith deriving (Eq, Show)
data Relation = Equals
              | GreaterThan
              | LessThan
              | GreaterThanOrEquals
              | LessThanOrEquals deriving (Eq, Show)
data Predicate = Predicate PredicateTerm Relation Arith (Maybe Unit)
  deriving (Eq, Show)
data Color = Green | Yellow | Red deriving (Eq, Show)
data CounterActionItem = Increment | Decrement | Reset deriving (Eq, Show)
data OtherActionItem = NoAction
                     | ProcessorReset
                     | ImageProcessReset
                     | FpeReset
                     | CameraOff
                     | CameraOn
                     | DisableRecorder
                     | EnableRecorder
                     | RunProgram
                     | SpawnProgram
                     deriving (Eq, Show)
data Action = CounterAction (Maybe Color) CounterActionItem CounterName
            | OtherAction OtherActionItem Integer
            deriving (Eq, Show)
