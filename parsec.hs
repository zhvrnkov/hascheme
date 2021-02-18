import Text.Parsec 

type Parser a = Parsec String () a

pword :: Parser String
pword = do
  x <- many1 $ noneOf ['(', ')', ' ']
  return x

pnum :: Parser Int
pnum = do
  num <- parsecMap read (many1 digit)
  notFollowedBy letter
  return num

pwords :: Parser [String]
pwords = do
  xs <- sepBy pword space
  return xs

data Exp = EAtom Atom | EList [Exp]
  deriving (Show, Eq)

data Atom = Number Int | Symbol String
  deriving (Show, Eq)

eatom :: Parser Exp
eatom = parsecMap EAtom atom

atom :: Parser Atom
atom = choice [number, symbol]
  where number = parsecMap Number $ try pnum
        symbol = parsecMap Symbol pword

elist :: Parser Exp
elist = (between (char '(') (char ')') (parsecMap EList (sepBy (eatom <|> elist) space)))

