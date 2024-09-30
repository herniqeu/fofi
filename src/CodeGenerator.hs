module CodeGenerator where

import Parser (AST(..), Expression(..))

generate :: AST -> String
generate (Program desc varDecls stmts) =
    "// " ++ desc ++ "\n\n" ++
    generateVarDecls varDecls ++
    generateStatements stmts

generateVarDecls :: [VarDecl] -> String
generateVarDecls = concatMap (\(VarDecl typ ids) -> 
    concatMap (\id -> "let " ++ id ++ " = " ++ initValue typ ++ ";\n") ids)
  where
    initValue "binario" = "false"
    initValue "numero" = "0"
    initValue "texto" = "\"\""

generateStatements :: [Statement] -> String
generateStatements = concatMap generateStatement

generateStatement :: Statement -> String
generateStatement (AssignStatement id expr) =
    id ++ " = " ++ generateExpr expr ++ ";\n"
generateStatement (IfStatement cond thenStmts elseStmts) =
    "if (" ++ generateExpr cond ++ ") {\n" ++
    generateStatements thenStmts ++
    "}" ++
    maybe "" (\stmts -> " else {\n" ++ generateStatements stmts ++ "}") elseStmts ++
    "\n"
generateStatement (WhileStatement cond stmts) =
    "while (" ++ generateExpr cond ++ ") {\n" ++
    generateStatements stmts ++
    "}\n"
generateStatement (RepeatStatement count stmts) =
    "for (let _i = 0; _i < " ++ generateExpr count ++ "; _i++) {\n" ++
    generateStatements stmts ++
    "}\n"
generateStatement (CommandStatement func args) =
    func ++ "(" ++ concatMap generateExpr args ++ ");\n"

generateExpr :: Expression -> String
generateExpr (BinaryOp op left right) =
    "(" ++ generateExpr left ++ " " ++ translateOp op ++ " " ++ generateExpr right ++ ")"
generateExpr (UnaryOp op expr) =
    translateUnaryOp op ++ "(" ++ generateExpr expr ++ ")"
generateExpr (Literal val) = val
generateExpr (Identifier id) = id
generateExpr (FunctionCall func args) =
    func ++ "(" ++ concatMap generateExpr args ++ ")"

translateOp :: String -> String
translateOp "e" = "&&"
translateOp "ou" = "||"
translateOp op = op

translateUnaryOp :: String -> String
translateUnaryOp "nao" = "!"
translateUnaryOp op = op