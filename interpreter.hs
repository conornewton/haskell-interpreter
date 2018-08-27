import Data.Char
import Text.Read (readMaybe)

data Op = Plus | Multiply
          deriving (Show)


data Command = Number Double |
               Operation Command Op Command
               deriving (Show)

parseCommandDouble :: String -> Maybe Double
parseCommandDouble [] = Nothing
parseCommandDouble xs = readMaybe $ takeWhile isDigit xs


parseCommand :: String -> Maybe Command
parseCommand [] = Nothing
parseCommand (x:'+':xs) = case parseCommand [x] of
                              Just c1 -> case parseCommand xs of
                                              Just c2 -> Just (Operation c2 Plus c2)
                                              Nothing -> Nothing
                              Nothing -> Nothing
parseCommand (x:'*':xs) = case parseCommand [x] of
                              Just c1 -> case parseCommand xs of
                                              Just c2 -> Just (Operation c2 Multiply c2)
                                              Nothing -> Nothing
                              Nothing -> Nothing
parseCommand xs = case parseCommandDouble xs of
                    Just x -> Just (Number x)
                    Nothing -> Nothing

evalCommand :: Command -> Double
evalCommand (Number x) = x
evalCommand (Operation c1 op c2) = evalOp op (evalCommand c1)  (evalCommand c2)
    where evalOp :: Op -> Double -> Double -> Double
          evalOp Plus x y= x + y
          evalOp Multiply x y= x * y

main :: IO ()
main = do
        input <-getLine
        case parseCommand $ filter (\x -> not $ isSpace x) input of
            Just c -> putStrLn $ "=" ++ (show $ evalCommand c)
            Nothing -> putStrLn "error"
        main

