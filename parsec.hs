import Text.Parsec 

type Parser a = Parsec String () a

pword :: Parser String
pword = do
  x <- many1 alphaNum
  return x

pwords :: Parser [String]
pwords = do
  xs <- sepBy pword space
  return xs

data Exp = Exp String | List [Exp]
  deriving Show

eword = parsecMap Exp pword

plist :: Parser [String]
plist = (between (char '(') (char ')') plist) <|> (sepBy (pword <|> (parsecMap concat plist)) space)

elist :: Parser Exp
elist = (between (char '(') (char ')') elist) <|> (parsecMap List (sepBy (eword <|> elist) space))

