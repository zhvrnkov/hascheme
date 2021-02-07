import qualified Data.Map as M
import Data.Maybe
import Text.ParserCombinators.ReadP
import Data.List
import Control.Applicative((<|>))

data Env = Env { values :: (M.Map String String)
               , parent :: (Maybe Env)
               }

data Exp = EAtom Atom | List [Exp]
  deriving (Eq, Show)

data Atom = INumber Int | FNumber Float | Symbol String
  deriving (Eq, Show)

data Token = String String | ListStart | ListEnd
  deriving (Eq, Show)

instance Read Exp where
  readsPrec index = (readP_to_S (list <|> atom)) . salt_input
    where openBracket  = char '('
          closeBracket = char ')'
          list         = fmap List $ openBracket >> (manyTill (readS_to_P (reads :: ReadS Exp)) closeBracket) 
          atom         = fmap EAtom $ readS_to_P $ (reads :: ReadS Atom)

instance Read Atom where
  readsPrec index input 
    | null input = []
    | otherwise = [(atom, intercalate " " xs)]
    where x:xs    = words input
          atom    = maybe (maybe (symbol (x, "")) (fnumber) float) (inumber) int
          int     = listToMaybe (readsPrec index x :: [(Int, String)])
          float   = listToMaybe (readsPrec index x :: [(Float, String)])
          inumber = INumber . fst
          fnumber = FNumber . fst
          symbol  = Symbol  . fst

main = interact repl

repl :: String -> String
repl = undefined

global_env :: Env
global_env = Env M.empty Nothing

salt_input :: String -> String
salt_input = concat . map paren
  where paren '(' = "( "
        paren ')' = " )"
        paren c   = [c]

