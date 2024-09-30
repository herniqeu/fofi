module SemanticAnalyzer where

import qualified Data.Map as Map
import Parser (AST(..), Expression(..))

type SymbolTable = Map.Map String String

analyze :: AST -> Either String ()
analyze (Program _ varDecls stmts) = do
    symbolTable <- buildSymbolTable varDecls
    analyzeStatements symbolTable stmts

buildSymbolTable :: [VarDecl] -> Either String SymbolTable
buildSymbolTable = foldl addVarDecl (Right Map.empty)
  where
    addVarDecl (Right table) (VarDecl typ ids) =
        foldl (\t id -> case t of
            Right m -> if Map.member id m
                then Left $ "Duplicate variable declaration: " ++ id
                else Right $ Map.insert id typ m
            Left err -> Left err) (Right table) ids
    addVarDecl (Left err) _ = Left err

analyzeStatements :: SymbolTable -> [Statement] -> Either String ()
analyzeStatements table = mapM_ (analyzeStatement table)

analyzeStatement :: SymbolTable -> Statement -> Either String ()
analyzeStatement table (AssignStatement id expr) = do
    exprType <- typeCheck table expr
    case Map.lookup id table of
        Just varType -> if varType == exprType || (varType == "numero" && exprType == "binario")
                        then Right ()
                        else Left $ "Type mismatch in assignment to " ++ id
        Nothing -> Left $ "Undeclared variable: " ++ id
analyzeStatement table (IfStatement cond thenStmts elseStmts) = do
    condType <- typeCheck table cond
    if condType /= "binario"
        then Left "Condition must be boolean"
        else do
            analyzeStatements table thenStmts
            case elseStmts of
                Just stmts -> analyzeStatements table stmts
                Nothing -> Right ()
analyzeStatement table (WhileStatement cond stmts) = do
    condType <- typeCheck table cond
    if condType /= "binario"
        then Left "Condition must be boolean"
        else analyzeStatements table stmts
analyzeStatement table (RepeatStatement count stmts) = do
    countType <- typeCheck table count
    if countType /= "numero"
        then Left "Repeat count must be a number"
        else analyzeStatements table stmts
analyzeStatement table (CommandStatement func args) =
    mapM_ (typeCheck table) args

typeCheck :: SymbolTable -> Expression -> Either String String
typeCheck table (BinaryOp op left right) = do
    leftType <- typeCheck table left
    rightType <- typeCheck table right
    case op of
        "+" -> if leftType == "numero" && rightType == "numero"
               then Right "numero"
               else Left "Arithmetic operation requires numbers"
        "-" -> if leftType == "numero" && rightType == "numero"
               then Right "numero"
               else Left "Arithmetic operation requires numbers"
        "*" -> if leftType == "numero" && rightType == "numero"
               then Right "numero"
               else Left "Arithmetic operation requires numbers"
        "/" -> if leftType == "numero" && rightType == "numero"
               then Right "numero"
               else Left "Arithmetic operation requires numbers"
        "%" -> if leftType == "numero" && rightType == "numero"
               then Right "numero"
               else Left "Arithmetic operation requires numbers"
        "^" -> if leftType == "numero" && rightType == "numero"
               then Right "numero"
               else Left "Arithmetic operation requires numbers"
        "=" -> if leftType == rightType
               then Right "binario"
               else Left "Comparison requires same types"
        "!=" -> if leftType == rightType
                then Right "binario"
                else Left "Comparison requires same types"
        "<" -> if leftType == "numero" && rightType == "numero"
               then Right "binario"
               else Left "Comparison requires numbers"
        "<=" -> if leftType == "numero" && rightType == "numero"
                then Right "binario"
                else Left "Comparison requires numbers"
        ">" -> if leftType == "numero" && rightType == "numero"
               then Right "binario"
               else Left "Comparison requires numbers"
        ">=" -> if leftType == "numero" && rightType == "numero"
                then Right "binario"
                else Left "Comparison requires numbers"
        "e" -> if leftType == "binario" && rightType == "binario"
               then Right "binario"
               else Left "Logical operation requires booleans"
        "ou" -> if leftType == "binario" && rightType == "binario"
                then Right "binario"
                else Left "Logical operation requires booleans"
        _ -> Left $ "Unknown operator: " ++ op
typeCheck table (UnaryOp op expr) = do
    exprType <- typeCheck table expr
    case op of
        "+" -> if exprType == "numero"
               then Right "numero"
               else Left "Unary plus requires a number"
        "-" -> if exprType == "numero"
               then Right "numero"
               else Left "Unary minus requires a number"
        "nao" -> if exprType == "binario"
                 then Right "binario"
                 else Left "Logical not requires a boolean"
        _ -> Left $ "Unknown unary operator: " ++ op
typeCheck _ (Literal val)
    | val `elem` ["v", "f"] = Right "binario"
    | all (`elem` ['0'..'9']) val = Right "numero"
    | otherwise = Right "texto"
typeCheck table (Identifier id) =
    case Map.lookup id table of
        Just t -> Right t
        Nothing -> Left $ "Undeclared variable: " ++ id
typeCheck table (FunctionCall func args) =
    case func of
        "ler_numero" -> Right "numero"
        "ler_binario" -> Right "binario"
        "ler" -> Right "numero"
        "consultar" -> Right "numero"
        "criar_figura" -> Right "numero"
        "criar_imagem" -> Right "numero"
        "colidiu" -> Right "binario"
        "aleatorio" -> Right "numero"
        _ -> Left $ "Unknown function: " ++ func