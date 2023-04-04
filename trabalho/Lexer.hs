module Lexer where

import Data.Char

data Ty = TBool
        | TNum
        | TFun Ty Ty
        deriving (Show, Eq)

data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | And Expr Expr
          | If Expr Expr Expr
          | Var String
          | Lam String Ty Expr
          | App Expr Expr
          | Paren Expr
          | Eq Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Or Expr Expr
          | Not Expr
          | Less Expr Expr
          | Greater Expr Expr
          | Let Expr Expr
          deriving (Show, Eq)

instance Ord Expr where
    compare (Num x) (Num y) = compare x y
    compare (BTrue) (BTrue) = EQ
    compare (BFalse) (BFalse) = EQ
    compare _ _ = error "deu erro!"

data Token = TokenTrue
           | TokenFalse
           | TokenNum Int
           | TokenAdd
           | TokenAnd
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenVar String
           | TokenLam
           | TokenColon
           | TokenArrow 
           | TokenLParen
           | TokenRParen
           | TokenBoolean
           | TokenNumber
           | TokenEq
           | TokenSub
           | TokenMult
           | TokenNot
           | TokenOr
           | TokenLess
           | TokenGreater
           | TokenLet
           deriving Show

isToken :: Char -> Bool
isToken c = elem c "->&|=*!<"

lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs)  = TokenAdd    : lexer cs
lexer ('\\':cs) = TokenLam    : lexer cs
lexer (':':cs)  = TokenColon  : lexer cs
lexer ('(':cs)  = TokenLParen : lexer cs
lexer (')':cs)  = TokenRParen : lexer cs
lexer (c:cs)
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlpha c = lexKW (c:cs)
    | isToken c = lexSymbol (c:cs)

lexNum :: String -> [Token]
lexNum cs =
    case span isDigit cs of
        (num, rest) -> TokenNum (read num) : lexer rest

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of
    ("true",   rest) -> TokenTrue    : lexer rest
    ("false",  rest) -> TokenFalse   : lexer rest
    ("if",     rest) -> TokenIf      : lexer rest
    ("then",   rest) -> TokenThen    : lexer rest
    ("else",   rest) -> TokenElse    : lexer rest
    ("Bool",   rest) -> TokenBoolean : lexer rest
    ("Number", rest) -> TokenNumber  : lexer rest
    ("let",    rest) -> TokenLet     : lexer rest
    (var,      rest) -> TokenVar var : lexer rest

lexSymbol :: String -> [Token]
lexSymbol cs = case span isToken cs of
    ("->", rest)  -> TokenArrow     : lexer rest
    ("&&", rest)  -> TokenAnd       : lexer rest
    ("==", rest)  -> TokenEq        : lexer rest
    ("-",  rest)  -> TokenSub       : lexer rest
    ("*",  rest)  -> TokenMult      : lexer rest
    ("!",  rest)  -> TokenNot       : lexer rest
    ("||", rest)  -> TokenOr        : lexer rest
    ("<<", rest)  -> TokenLess      : lexer rest
    (">>", rest)  -> TokenGreater   : lexer rest
    _             -> error "Lexical error: unexpected symbol"