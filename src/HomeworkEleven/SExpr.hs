module HomeworkEleven.SExpr where

import HomeworkTen.AParser
import Control.Applicative
import Data.Char

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident 
  = String

-- An "atom" is either an integer value or an identifier.
data Atom 
  = N Integer 
  | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr 
  = A Atom
  | Comb [SExpr]
  deriving (Show)

instance Eq SExpr where
    (==) (A (N n1)) (A (N n2)) = n1 == n2
    (==) (A (I i1)) (A (I i2)) = i1 == i2
    (==) (Comb c1) (Comb c2) = and $ zipWith (==) c1 c2
    (==) _ _ = False

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore (Parser p) = 
  let f str = 
        case p str of
          Nothing -> Just([],str)
          Just (x,r) -> first (x:) <$> f r
  in Parser f

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p) 

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = 
  let alpha = oneOrMore (satisfy isAlpha)
      alphaNum = zeroOrMore (satisfy isAlphaNum)
  in liftA2 (++) alpha alphaNum

parseSExpr :: Parser SExpr
parseSExpr = 
  let single = A <$> (I <$> ident <|> N <$> posInt)
      comb = Comb <$> (char '(' *> many parseSExpr <* char ')')
  in spaces *> (single <|> comb) <* spaces