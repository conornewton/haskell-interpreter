module Spec where

data Op = Plus | Multiply | Divide | Subtract deriving (Show)

data Command  = Variable String MathCommand | Evaluate MathCommand deriving (Show)

data MathCommand = VariableUsage String |
                   FunctionUsage String [MathCommand] |
                   Number Double |
                   Operation MathCommand Op MathCommand
                    deriving (Show)
