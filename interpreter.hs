import Data.Char
import Text.Read (readMaybe)

data Op = Plus | Multiply deriving (Show)


data Command = Number Integer |
               Operation Command Op Command |
               Error deriving (Show)

parseCommandInteger :: String -> Maybe Integer
parseCommandInteger [] = Nothing
parseCommandInteger xs = readMaybe $ takeWhile isDigit xs


parseCommand :: String -> Command
parseCommand [] = Error
parseCommand (x:'+':xs) = Operation (parseCommand [x]) Plus (parseCommand xs)
parseCommand (x:'*':xs) = Operation (parseCommand [x]) Multiply (parseCommand xs)
parseCommand xs = case parseCommandInteger xs of
                    Just x -> Number x
                    Nothing -> Error

evalCommand :: Command -> Integer
evalCommand (Number x) = x
evalCommand (Operation c1 op c2) = evalOp op (evalCommand c1)  (evalCommand c2)
    where evalOp :: Op -> Integer -> Integer -> Integer
          evalOp Plus x y= x + y
          evalOp Multiply x y= x * y

main :: IO ()
main = do
        input <-getLine
        putStrLn $ "=" ++ (show $ evalCommand $ parseCommand $ filter (\x -> not $ isSpace x) input)
        main

