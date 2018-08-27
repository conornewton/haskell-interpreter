import Prelude hiding (lookup)
import System.IO
import Data.Map.Strict (Map, empty, lookup, insert)
import Data.Char
import Text.Read (readMaybe)

data Op = Plus | Multiply

data Command  = Variable String MathCommand | Evaluate MathCommand

data MathCommand = VariableUsage String |
                   Number Double |
                   Operation MathCommand Op MathCommand

-- tokenise
split :: String -> [String]
split []       = []
split ('+':xs) = ["+"] ++ (split xs)
split ('*':xs) = ["*"] ++ (split xs)
split ('=':xs) = ["="] ++ (split xs)
split (' ':xs) = split xs
split xs       = if isDigit $ head xs then [(takeWhile isDigit xs)] ++ (split (dropWhile isDigit xs)) else [(takeWhile isAlpha xs)] ++ (split (dropWhile isAlpha xs))

checkTokens :: String -> Bool
checkTokens [] = True
checkTokens (x:xs) = if elem x valid then checkTokens xs else False
    where valid = "0123456789abcdefhijklmnopqrstuvwxyz+=*"

parseCommand :: [String] -> Maybe Command
parseCommand (id:"=":xs) = do m <- parseMathCommand xs
                              Just (Variable id m)
parseCommand xs = do m <- parseMathCommand xs
                     Just (Evaluate m)

parseMathCommand :: [String] -> Maybe MathCommand
parseMathCommand [] = Nothing
parseMathCommand (x:"+":xs) = do c1 <- parseMathCommand [x]
                                 c2 <- parseMathCommand xs
                                 Just (Operation c1 Plus c2)
parseMathCommand (x:"*":xs) = do c1 <- parseMathCommand [x]
                                 c2 <- parseMathCommand xs
                                 Just (Operation c1 Multiply c2)
parseMathCommand (x:xs) = case readMaybe x of
                             Just d -> Just (Number d)
                             Nothing -> Just (VariableUsage x)

evalCommand :: Command -> (Map String Double) -> Either (Maybe Double) (Map String Double)
evalCommand (Variable id cmd) m = Right (case evalMathCommand cmd m of 
                                              Just d -> insert id d m
                                              Nothing -> m)
evalCommand (Evaluate cmd) m = Left (do e <- evalMathCommand cmd m
                                        Just e)

evalMathCommand :: MathCommand -> Map String Double -> Maybe Double
evalMathCommand (Number x) m = Just x
evalMathCommand (VariableUsage id) m = lookup id m
evalMathCommand (Operation c1 op c2) m = do r1 <- (evalMathCommand c1 m) 
                                            r2 <- (evalMathCommand c2 m)
                                            Just (evalOp op r1 r2)
    where evalOp :: Op -> Double -> Double -> Double
          evalOp Plus x y = x + y
          evalOp Multiply x y = x * y

repl :: Map String Double -> IO ()
repl m = do putStr ">"
            hFlush stdout
            input <- getLine
            case checkTokens input of
                True -> case parseCommand $ split input of
                        Just c -> case  evalCommand c m of
                            Left c -> case c of
                                      Just c -> putStrLn $ "=" ++ (show c)
                                      Nothing -> putStrLn "eval error!"
                            Right m' -> putStrLn "added variable" >> repl m'
                        Nothing -> putStrLn "parse error!"
                False -> putStrLn "token error!"
            repl m

main :: IO ()
main = repl empty
