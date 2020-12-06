import qualified Data.Map as M
import Data.Maybe

data Env = Env { values :: (M.Map String String)
               , parent :: (Maybe Env)
               }

data Exp = Atom Atom | List [Exp]
  deriving (Show)
data Atom = INumber Int | FNumber Float | Symbol String
  deriving (Eq, Show)

instance Read Atom where
  readsPrec index input = [(atom, "")]
    where atom    = maybe (maybe (symbol (input, "")) (fnumber) float) (inumber) int
          int     = listToMaybe (readsPrec index input :: [(Int, String)])
          float   = listToMaybe (readsPrec index input :: [(Float, String)])
          inumber = INumber . fst
          fnumber = FNumber . fst
          symbol  = Symbol . fst

main = interact repl

repl :: String -> String
repl = undefined

global_env :: Env
global_env = Env M.empty Nothing

tokenize :: String -> [String]
tokenize = words . concat . map paren
  where paren '(' = " ( "
        paren ')' = " ) "
        paren c   = [c]

parse :: [String] -> Exp
parse (x:xs) = undefined

eval :: Exp -> Env -> Env
eval = undefined
