module Parser where
import Spec
import Text.Read (readMaybe)

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
parseMathCommand (x:"/":xs) = do c1 <- parseMathCommand [x]
                                 c2 <- parseMathCommand xs
                                 Just (Operation c1 Divide c2)
parseMathCommand (x:"-":xs) = do c1 <- parseMathCommand [x]
                                 c2 <- parseMathCommand xs
                                 Just (Operation c1 Subtract c2)
parseMathCommand (x:[]) = case readMaybe x of
                             Just d -> Just (Number d)
                             Nothing -> Just (VariableUsage x)
parseMathCommand (x:xs) = case readMaybe x of
                             Just d -> Just (Number d)
                             Nothing -> case head xs == "(" of
                                        True ->  do params <- sequence (map parseMathCommand (parseParam $ takeWhile (/=")") (tail xs)))
                                                    Just (FunctionUsage x params)
                                        False -> Just (VariableUsage x)

splitOn :: [String] -> String -> ([String], [String])
splitOn [] s = ([], [])
splitOn xs s = (takeWhile (/=s) xs, tail $ dropWhile (/=s)  xs)

parseParam :: [String] -> [[String]]
parseParam [] = []
parseParam (x:[]) = [[x]]
parseParam xs = [a] ++ parseParam b
        where (a,b) = splitOn xs "," 
