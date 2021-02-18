module Parser where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)

type Parser a = Parsec String () a

data Exp = EAtom Atom | EList [Exp]
  deriving (Show, Eq)

data Atom = Number Int | Symbol String
  deriving (Show, Eq)

parse :: String -> Either ParseError Exp
parse = P.parse elist ""

elist :: Parser Exp
elist = (between (char '(') (char ')') (parsecMap EList (sepBy (eatom <|> elist) space)))

pword :: Parser String
pword = do
  x <- many1 $ noneOf ['(', ')', ' ']
  return x

pnum :: Parser Int
pnum = do
  num <- parsecMap read (many1 digit)
  notFollowedBy letter
  return num

eatom :: Parser Exp
eatom = parsecMap EAtom atom

atom :: Parser Atom
atom = choice [number, symbol]
  where number = parsecMap Number $ try pnum
        symbol = parsecMap Symbol pword

