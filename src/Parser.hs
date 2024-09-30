module Parser where

import Lexer (Token(..))

data AST = Program String [VarDecl] [Statement]
         | VarDecl String [String]
         | AssignStatement String Expression
         | IfStatement Expression [Statement] (Maybe [Statement])
         | WhileStatement Expression [Statement]
         | RepeatStatement Expression [Statement]
         | CommandStatement String [Expression]
         deriving (Show)

data Expression = BinaryOp String Expression Expression
                | UnaryOp String Expression
                | Literal String
                | Identifier String
                | FunctionCall String [Expression]
                deriving (Show)

parse :: [Token] -> Either String AST
parse tokens = case parseProgram tokens of
    (Right ast, []) -> Right ast
    (_, t:_) -> Left $ "Unexpected token: " ++ show t

parseProgram :: [Token] -> (Either String AST, [Token])
parseProgram (Token "PROGRAMA" _ _:Token "STR" desc _:Token "VAR" _ _:ts) =
    case parseVarDecls ts of
        (Right varDecls, Token "LBLOCK" _ _:rest) ->
            case parseStatements rest of
                (Right stmts, Token "RBLOCK" _ _:Token "EOF" _ _:remaining) ->
                    (Right $ Program desc varDecls stmts, remaining)
                _ -> (Left "Expected statements and closing brace", ts)
        _ -> (Left "Expected variable declarations", ts)
parseProgram _ = (Left "Invalid program structure", [])

parseVarDecls :: [Token] -> (Either String [VarDecl], [Token])
parseVarDecls ts = parseVarDeclsHelper ts []

parseVarDeclsHelper :: [Token] -> [VarDecl] -> (Either String [VarDecl], [Token])
parseVarDeclsHelper (Token "TYPE" typ _:Token "ID" id _:ts) acc =
    case ts of
        (Token "COMMA" _ _:rest) -> parseVarDeclsHelper rest (VarDecl typ [id]:acc)
        (Token "SEMICOLON" _ _:rest) -> parseVarDeclsHelper rest (VarDecl typ [id]:acc)
        _ -> (Left "Invalid variable declaration", ts)
parseVarDeclsHelper ts acc = (Right (reverse acc), ts)

parseStatements :: [Token] -> (Either String [Statement], [Token])
parseStatements ts = parseStatementsHelper ts []

parseStatementsHelper :: [Token] -> [Statement] -> (Either String [Statement], [Token])
parseStatementsHelper (Token "RBLOCK" _ _:ts) acc = (Right (reverse acc), Token "RBLOCK" _ _:ts)
parseStatementsHelper ts acc =
    case parseStatement ts of
        (Right stmt, rest) -> parseStatementsHelper rest (stmt:acc)
        (Left err, _) -> (Left err, ts)

parseStatement :: [Token] -> (Either String Statement, [Token])
parseStatement (Token "ID" id _:Token "ASSIGN" _ _:ts) =
    case parseExpression ts of
        (Right expr, Token "SEMICOLON" _ _:rest) -> (Right $ AssignStatement id expr, rest)
        _ -> (Left "Invalid assignment statement", ts)
parseStatement (Token "SE" _ _:Token "LPAR" _ _:ts) =
    case parseExpression ts of
        (Right cond, Token "RPAR" _ _:Token "LBLOCK" _ _:rest) ->
            case parseStatements rest of
                (Right thenStmts, Token "RBLOCK" _ _:Token "SENAO" _ _:Token "LBLOCK" _ _:elseRest) ->
                    case parseStatements elseRest of
                        (Right elseStmts, Token "RBLOCK" _ _:remaining) ->
                            (Right $ IfStatement cond thenStmts (Just elseStmts), remaining)
                        _ -> (Left "Invalid else block", elseRest)
                (Right thenStmts, Token "RBLOCK" _ _:remaining) ->
                    (Right $ IfStatement cond thenStmts Nothing, remaining)
                _ -> (Left "Invalid if statement", rest)
        _ -> (Left "Invalid if condition", ts)
parseStatement (Token "ENQUANTO" _ _:Token "LPAR" _ _:ts) =
    case parseExpression ts of
        (Right cond, Token "RPAR" _ _:Token "LBLOCK" _ _:rest) ->
            case parseStatements rest of
                (Right stmts, Token "RBLOCK" _ _:remaining) ->
                    (Right $ WhileStatement cond stmts, remaining)
                _ -> (Left "Invalid while statement", rest)
        _ -> (Left "Invalid while condition", ts)
parseStatement (Token "REPITA" _ _:Token "LPAR" _ _:ts) =
    case parseExpression ts of
        (Right count, Token "RPAR" _ _:Token "LBLOCK" _ _:rest) ->
            case parseStatements rest of
                (Right stmts, Token "RBLOCK" _ _:remaining) ->
                    (Right $ RepeatStatement count stmts, remaining)
                _ -> (Left "Invalid repeat statement", rest)
        _ -> (Left "Invalid repeat count", ts)
parseStatement (Token "ID" func _:Token "LPAR" _ _:ts) =
    case parseExpressionList ts of
        (Right args, Token "RPAR" _ _:Token "SEMICOLON" _ _:rest) ->
            (Right $ CommandStatement func args, rest)
        _ -> (Left "Invalid function call", ts)
parseStatement _ = (Left "Invalid statement", [])

parseExpression :: [Token] -> (Either String Expression, [Token])
parseExpression = parseRelationalExpr

parseRelationalExpr :: [Token] -> (Either String Expression, [Token])
parseRelationalExpr ts =
    case parseSumExpr ts of
        (Right left, Token "OPREL" op _:rest) ->
            case parseSumExpr rest of
                (Right right, remaining) -> (Right $ BinaryOp op left right, remaining)
                err -> err
        result -> result

parseSumExpr :: [Token] -> (Either String Expression, [Token])
parseSumExpr ts =
    case parseMultExpr ts of
        (Right left, Token "OPSUM" op _:rest) ->
            case parseSumExpr rest of
                (Right right, remaining) -> (Right $ BinaryOp op left right, remaining)
                err -> err
        result -> result

parseMultExpr :: [Token] -> (Either String Expression, [Token])
parseMultExpr ts =
    case parsePowerExpr ts of
        (Right left, Token "OPMUL" op _:rest) ->
            case parseMultExpr rest of
                (Right right, remaining) -> (Right $ BinaryOp op left right, remaining)
                err -> err
        result -> result

parsePowerExpr :: [Token] -> (Either String Expression, [Token])
parsePowerExpr ts =
    case parseUnaryExpr ts of
        (Right left, Token "OPPOW" _:rest) ->
            case parsePowerExpr rest of
                (Right right, remaining) -> (Right $ BinaryOp "^" left right, remaining)
                err -> err
        result -> result

parseUnaryExpr :: [Token] -> (Either String Expression, [Token])
parseUnaryExpr (Token "OPSUM" op _:ts) =
    case parseUnaryExpr ts of
        (Right expr, rest) -> (Right $ UnaryOp op expr, rest)
        err -> err
parseUnaryExpr (Token "NAO" _ _:ts) =
    case parseUnaryExpr ts of
        (Right expr, rest) -> (Right $ UnaryOp "nao" expr, rest)
        err -> err
parseUnaryExpr ts = parseFactor ts

parseFactor :: [Token] -> (Either String Expression, [Token])
parseFactor (Token "ID" id _:ts) = (Right $ Identifier id, ts)
parseFactor (Token "INT" val _:ts) = (Right $ Literal val, ts)
parseFactor (Token "BOOL" val _:ts) = (Right $ Literal val, ts)
parseFactor (Token "LPAR" _ _:ts) =
    case parseExpression ts of
        (Right expr, Token "RPAR" _ _:rest) -> (Right expr, rest)
        _ -> (Left "Unmatched parenthesis", ts)
parseFactor (Token "ID" func _:Token "LPAR" _ _:ts) =
    case parseExpressionList ts of
        (Right args, Token "RPAR" _ _:rest) -> (Right $ FunctionCall func args, rest)
        _ -> (Left "Invalid function call", ts)
parseFactor _ = (Left "Invalid factor", [])

parseExpressionList :: [Token] -> (Either String [Expression], [Token])
parseExpressionList ts = parseExprListHelper ts []

parseExprListHelper :: [Token] -> [Expression] -> (Either String [Expression], [Token])
parseExprListHelper (Token "RPAR" _ _:ts) acc = (Right (reverse acc), Token "RPAR" _ _:ts)
parseExprListHelper ts acc =
    case parseExpression ts of
        (Right expr, Token "COMMA" _ _:rest) -> parseExprListHelper rest (expr:acc)
        (Right expr, rest) -> (Right (reverse (expr:acc)), rest)
        (Left err, _) -> (Left err, ts)