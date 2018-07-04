{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module HomeworkFive.Calc where

import HomeworkFive.Parser
import HomeworkFive.StackVM

data ExprT 
    = Lit Integer
    | Add ExprT ExprT
    | Mul ExprT ExprT
    deriving (Show, Eq)

class Expr a where
    lit  :: Integer -> a
    add' :: a -> a -> a
    mul  :: a -> a -> a

instance Expr ExprT where
    lit  = Lit
    add' = Add
    mul  = Mul

instance Expr Integer where
    lit      = id
    add' a b = a + b
    mul a b  = a * b

instance Expr Bool where
    lit      = (> 0)
    add' a b = a || b
    mul a b  = a && b

newtype MinMax 
    = MinMax Integer deriving (Eq, Show, Ord)    

instance Expr MinMax where
    lit  = MinMax
    add' = max
    mul  = min

newtype Mod7
    = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit n                  = Mod7 (n `mod` 7)
    add' (Mod7 a) (Mod7 b) = lit (a + b)
    mul (Mod7 a) (Mod7 b)  = lit (a * b)

instance Expr Program where
    lit n    = [PushI n]
    add' a b = a ++ b ++ [AddVM]
    mul a b  = a ++ b ++ [MulVM]

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = 
    let me = parseExp Lit Add Mul s 
    in case me of
        (Just e) -> Just (eval e)
        _        -> Nothing

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a 
testExp = parseExp lit add' mul "(3 * -4) + 5"

compile :: String -> Maybe Program
compile e = parseExp lit add' mul e :: Maybe Program