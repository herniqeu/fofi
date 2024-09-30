module Main where

import System.Environment (getArgs)
import Lexer (tokenize)
import Parser (parse)
import SemanticAnalyzer (analyze)
import CodeGenerator (generate)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> do
            contents <- readFile inputFile
            let tokens = tokenize contents
            case parse tokens of
                Left error -> putStrLn $ "Parse error: " ++ error
                Right ast -> do
                    case analyze ast of
                        Left error -> putStrLn $ "Semantic error: " ++ error
                        Right _ -> do
                            let code = generate ast
                            writeFile (inputFile ++ ".js") code
                            putStrLn "Compilation successful!"
        _ -> putStrLn "Usage: fofi-compiler <input-file>"