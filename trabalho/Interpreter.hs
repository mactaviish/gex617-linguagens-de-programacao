-- arvore de sintaxe abstrata
module Interpreter where

import Lexer
subst :: String -> Expr -> Expr -> Expr
subst x n b@(Var v) =
    if v == x then n else b
subst x n (Lam v t b)       = Lam v t (subst x n b)
subst x n (App e1 e2)       = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2)       = Add (subst x n e1) (subst x n e2)
subst x n (And e1 e2)       = And (subst x n e1) (subst x n e2)
subst x n (If e e1 e2)      = If (subst x n e) (subst x n e1) (subst x n e2)
subst x n (Paren e)         = Paren (subst x n e)
subst x n (Eq e1 e2)        = Eq (subst x n e1) (subst x n e2)
subst x n (Sub e1 e2)       = Sub (subst x n e1) (subst x n e2)
subst x n (Mult e1 e2)      = Mult (subst x n e1) (subst x n e2)
subst x n (Not e)           = Not (subst x n e)
subst x n (Or e1 e2)        = Or (subst x n e1) (subst x n e2)
subst x n (Less e1 e2)      = Less (subst x n e1) (subst x n e2)
subst x n (Greater e1 e2)   = Greater (subst x n e1) (subst x n e2)
subst x n e = e

isvalue :: Expr -> Bool
isvalue BTrue       = True
isvalue BFalse      = True
isvalue (Num _)     = True
isvalue (Lam _ _ _) = True
isvalue _           = False

-- funcao que avalia um passo de execucao
step :: Expr -> Maybe Expr

-- expressao add
step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
step (Add (Num n1) e2) =
    case (step e2) of
        Just e2' -> Just (Add (Num n1) e2')
        Nothing  -> Nothing
step (Add e1 e2) =
    case (step e1) of
        Just e1' -> Just (Add e1' e2)
        Nothing  -> Nothing

-- expressao and
step (And BTrue e2) = Just e2
step (And BFalse _) = Just BFalse
step (And e1 e2) =
    case (step e1) of
        Just e1' -> Just (And e1' e2)
        Nothing  -> Nothing

-- expressao if
step (If BTrue e1 _)  = Just e1
step (If BFalse _ e2) = Just e2
step (If e e1 e2) =
    case (step e1) of
        Just e1' -> Just (If e e1' e2)
        Nothing  -> Nothing

-- expressao app
step (App e1@(Lam x t b) e2)
    | isvalue e2 = Just (subst x e2 b)
    | otherwise = case step e2 of
        Just e2' -> Just (App e1 e2')
        Nothing  -> Nothing
step (App e1 e2) =
    case step e1 of
        Just e1' -> Just (App e1' e2)
        Nothing  -> Nothing

-- expressao paren
step (Paren e) = Just e

-- expressao equals
step (Eq e1 e2)
    | isvalue e1 && isvalue e2 =
        if (e1 == e2) then
            Just BTrue
        else
            Just BFalse
    | isvalue e1 =
        case step e2 of
            Just e2' -> Just (Eq e1 e2')
            Nothing  -> Nothing
    | otherwise =
        case step e1 of
            Just e1' -> Just (Eq e1' e2)
            Nothing  -> Nothing

-- expressao sub
step (Sub (Num n1) (Num n2)) = Just (Num (n1 - n2))
step (Sub (Num n1) e2) =
    case (step e2) of
        Just e2' -> Just (Sub (Num n1) e2')
        Nothing  -> Nothing
step (Sub e1 e2) =
    case (step e1) of
        Just e1' -> Just (Sub e1' e2)
        Nothing  -> Nothing

-- expressao mult
step (Mult (Num n1) (Num n2)) = Just (Num (n1 * n2))
step (Mult (Num n1) e2) =
    case (step e2) of
        Just e2' -> Just (Mult (Num n1) e2')
        Nothing  -> Nothing
step (Mult e1 e2) =
    case (step e1) of
        Just e1' -> Just (Mult e1' e2)
        Nothing  -> Nothing

-- expressao not
step (Not BFalse) = Just BTrue
step (Not BTrue)  = Just BFalse
step (Not e1) =
    case (step e1) of
        Just e1' -> Just (Not e1')
        Nothing  -> Nothing

-- expressao or
step (Or e1 BFalse) = Just e1
step (Or BFalse e2) = Just e2
step (Or e1 e2) =
    case (step e1) of
        Just e1' -> Just (Or e1' e2)
        Nothing  -> Nothing

-- separar o tipo igual no mult
-- expressao less
step (Less e1 e2)
    | isvalue e1 && isvalue e2 =
        if (e1 < e2) then
            Just BTrue
        else
            Just BFalse
    | isvalue e1 =
        case step e2 of
            Just e2' -> Just (Less e1 e2')
            Nothing  -> Nothing
    | otherwise =
        case step e1 of
            Just e1' -> Just (Less e1' e2)
            Nothing  -> Nothing

-- expressao Greater
step (Greater e1 e2)
    | isvalue e1 && isvalue e2 =
        if (e1 > e2) then
            Just BTrue
        else
            Just BFalse
    | isvalue e1 =
        case step e2 of
            Just e2' -> Just (Less e1 e2')
            Nothing  -> Nothing
    | otherwise =
        case step e1 of
            Just e1' -> Just (Less e1' e2)
            Nothing  -> Nothing

-- expressao default
step e = Just e

-- funcao eval
eval :: Expr -> Expr
eval e
    | isvalue e = e
    | otherwise =
        case step e of
            Just e' -> eval e'
            Nothing -> error "Interpreter error"