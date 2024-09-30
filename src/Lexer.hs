module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, toLower)

data Token = Token
    { tokenType :: String
    , tokenValue :: String
    , tokenLine :: Int
    } deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize input = tokenizeHelper input 1 []

tokenizeHelper :: String -> Int -> [Token] -> [Token]
tokenizeHelper [] _ tokens = reverse $ Token "EOF" "EOF" (length tokens + 1) : tokens
tokenizeHelper (c:cs) line tokens
    | c == ' ' || c == '\t' = tokenizeHelper cs line tokens
    | c == '\n' = tokenizeHelper cs (line + 1) tokens
    | c == '#' = tokenizeHelper (dropWhile (/= '\n') cs) line tokens
    | c == '/' && head cs == '#' = tokenizeHelper (dropComment cs) line tokens
    | isAlpha c = let (word, rest) = span isAlphaNum (c:cs) in
                  tokenizeHelper rest line (classifyWord (map toLower word) line : tokens)
    | isDigit c = let (num, rest) = span isDigit (c:cs) in
                  tokenizeHelper rest line (Token "INT" num line : tokens)
    | c == '"' = let (str, rest) = span (/= '"') cs in
                 tokenizeHelper (tail rest) line (Token "STR" str line : tokens)
    | otherwise = tokenizeHelper cs line (classifySymbol [c] line : tokens)

classifyWord :: String -> Int -> Token
classifyWord word line
    | word `elem` ["programa", "var", "se", "senao", "enquanto", "repita"] = Token (map toUpper word) word line
    | word `elem` ["binario", "numero", "texto"] = Token "TYPE" word line
    | word `elem` ["v", "f"] = Token "BOOL" word line
    | word == "nao" = Token "NAO" word line
    | otherwise = Token "ID" word line

classifySymbol :: String -> Int -> Token
classifySymbol sym line = case sym of
    ":" -> Token "ASSIGN" sym line
    "," -> Token "COMMA" sym line
    ";" -> Token "SEMICOLON" sym line
    "(" -> Token "LPAR" sym line
    ")" -> Token "RPAR" sym line
    "{" -> Token "LBLOCK" sym line
    "}" -> Token "RBLOCK" sym line
    "+" -> Token "OPSUM" sym line
    "-" -> Token "OPSUM" sym line
    "*" -> Token "OPMUL" sym line
    "/" -> Token "OPMUL" sym line
    "%" -> Token "OPMUL" sym line
    "^" -> Token "OPPOW" sym line
    "=" -> Token "OPREL" sym line
    "!=" -> Token "OPREL" sym line
    "<" -> Token "OPREL" sym line
    "<=" -> Token "OPREL" sym line
    ">" -> Token "OPREL" sym line
    ">=" -> Token "OPREL" sym line
    _ -> Token "UNKNOWN" sym line

dropComment :: String -> String
dropComment ('#':'/':cs) = cs
dropComment (_:cs) = dropComment cs
dropComment [] = []