import Prelude hiding (lookup)

import System.IO
import System.Exit
import Data.Map.Strict (Map, empty)
import Data.Char

import Parser
import Tokeniser
import Eval
import Spec

repl :: Map String MathCommand -> IO ()
repl m = do putStr ">"
            hFlush stdout
            input <- getLine
            case input == ":q" of
                True -> putStrLn "bye bye!" >> exitSuccess
                False -> do
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
main = putStrLn "CONE v0.01" >> putStrLn "Developed by Conor Newton" >> repl empty
