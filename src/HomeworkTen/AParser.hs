{-# LANGUAGE InstanceSigs #-}

module HomeworkTen.AParser where

import Data.Char
import Control.Applicative

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a 
    = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser s) = Parser (fmap (first f) . s)

instance Applicative Parser where
    pure a = Parser (\s -> Just(a,s))
    (<*>) (Parser f) p2 = Parser $ \s ->
        case f s of
            Nothing -> Nothing
            Just (a,s') -> runParser (a <$> p2) s'

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser f1) <|> (Parser f2) = Parser (\s -> f1 s <|> f2 s)

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
    where
    f xs
        | null ns   = Nothing
        | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a->b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

abParser :: Parser (Char, Char)
abParser =
    let f a b = (a,b)
    in liftA2 f (char 'a') (char 'b')

abParser' :: Parser ()
abParser' =
    let f _ _= ()
    in liftA2 f (char 'a') (char 'b')

intPair :: Parser [Integer]
intPair = 
    let f x _ z = [x,z]
    in liftA3 f posInt (char ' ') posInt

intOrUppercase :: Parser ()
intOrUppercase =
    let f _ = ()
        int = f <$> posInt
        upper = f <$> satisfy isUpper
    in int <|> upper

functionParser :: (a -> b) -> Parser(a -> b)
functionParser f = Parser (\s -> Just(f, s))