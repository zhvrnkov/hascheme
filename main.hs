import qualified Data.Map as M
import Data.Maybe

data Env = Env { values :: (M.Map String String)
               , parent :: (Maybe Env)
               }

data Exp = Atom Atom | List [Exp]
  deriving (Eq, Show)

data Token = String String | ListStart | ListEnd
  deriving (Eq, Show)

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

parse :: [String] -> [Exp]
parse list@(x:xs)
  | xs == [] = []
  | x == "(" = [List $ pxs]
  | x == ")" = []
  | otherwise = let px = (Atom . read $  x) in px:pxs
  where pxs = parse xs

parse_tokens :: [String] -> [Token]
parse_tokens [] = []
parse_tokens (x:xs) = token:(parse_tokens xs)
  where token = if x == "(" then ListStart
                else if x == ")" then ListEnd
                else String x

parse_list :: [String] -> [Exp]
parse_list (x:xs)
  | x == "(" = parse xs
  | x == ")" = []
  
eval :: [Exp] -> Env -> Env
eval (e:es) env = undefined

test_exp = "(define foo (param) (bar foo))"
test_tokens = tokenize test_exp
test_exp_parsed :: [Exp]
test_exp_parsed = [exp]
  where exp = List [Atom $ Symbol "define",
                    Atom $ Symbol "foo",
                    List [Atom $ Symbol "param"],
                    List [Atom $ Symbol "bar", Atom $ Symbol "foo"]]
λ> λ> 
