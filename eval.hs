module Eval where
import Prelude hiding (insert, lookup)
import Data.Map.Strict (Map, insert, lookup)
import Spec

evalCommand :: Command -> (Map String MathCommand) -> Either (Maybe Double) (Map String MathCommand)
evalCommand (Variable id cmd) m = Right (insert id cmd m)
evalCommand (Evaluate cmd) m = Left (do e <- evalMathCommand cmd m
                                        Just e)

evalMathCommand :: MathCommand -> Map String MathCommand -> Maybe Double
evalMathCommand (Number x) m = Just x
evalMathCommand (FunctionUsage id xs) m = do cmd <- lookup id m
                                             evalMathCommand cmd (paramIter xs m) 
evalMathCommand (VariableUsage id) m = do cmd <- lookup id m
                                          evalMathCommand cmd m
evalMathCommand (Operation c1 op c2) m = do r1 <- (evalMathCommand c1 m) 
                                            r2 <- (evalMathCommand c2 m)
                                            Just (evalOp op r1 r2)
evalOp :: Op -> Double -> Double -> Double
evalOp Plus x y = x + y
evalOp Multiply x y = x * y
evalOp Divide x y = x / y
evalOp Subtract x y = x - y

paramIter :: [MathCommand] -> (Map String MathCommand) -> (Map String MathCommand)
paramIter [] m = m
paramIter (x:xs) m = insert ("$" ++ (show $ length xs)) x (paramIter xs m)
