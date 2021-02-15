import qualified Data.Map as M
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Data.List as L
import Control.Applicative((<|>))

data Env = Env { values :: (M.Map String Atom)
               , parent :: (Maybe Env)
               } deriving Show

newtype EvalResult a = EvalResult { getResult :: (a, Env) }
  deriving Show

instance Functor EvalResult where
  fmap f (EvalResult (x, env)) = EvalResult $ (f x, env)

data Exp = EAtom Atom | List [Exp]
  deriving (Eq, Show)

data Atom = INumber Int | FNumber Float | Symbol String
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
    | otherwise = [(atom x, L.intercalate " " xs)]
    where x:xs    = words input
          atom    = fmap (fst . head) $ readP_to_S (int <++ float <++ symbol)
          int     = fmap INumber $ (readS_to_P (reads :: ReadS Int))
          float   = fmap FNumber $ (readS_to_P (reads :: ReadS Float))
          symbol  = fmap Symbol  $ ((readS_to_P (reads :: ReadS String)) <++ (readS_to_P name))

name :: String -> [(String, String)]
name input = [(x, L.intercalate " " xs)]
  where x:xs = words input
        
main = interact repl

exec = (eval global_env) . parse

parse :: String -> Exp
parse = read

eval :: Env -> Exp -> EvalResult Atom

eval env (EAtom int@(INumber _))   = EvalResult (int, env)
eval env (EAtom float@(FNumber _)) = EvalResult (float, env)
eval env (EAtom (Symbol symbol))   = EvalResult (env ! symbol, env)

eval env (List [EAtom (Symbol "if"), test, conseq, alt]) = EvalResult (result, nenv1)
  where (is_test_true, nenv) = (getResult . (fmap boolify) . (eval env)) $ test
        (result, nenv1)      = getResult $ if is_test_true 
                                    then eval nenv conseq
                                    else eval nenv alt

eval env (List [EAtom (Symbol "define"), (EAtom (Symbol symbol)), exp]) = EvalResult (INumber 1, new_env)
  where (result, nenv) = getResult $ eval env exp
        new_env        = insert nenv symbol result 

repl :: String -> String
repl = show . parse 

global_env :: Env
global_env = Env (M.fromList content) Nothing
  where content = [("foo", INumber 2)]

salt_input :: String -> String
salt_input = concat . map paren
  where paren '(' = "( "
        paren ')' = " )"
        paren c   = [c]

(!) :: Env -> String -> Atom
(!) env key = (M.!) (values env) key

insert :: Env -> String -> Atom -> Env
insert src key value = Env content (parent src)
  where content = M.insert key value (values src)

boolify :: Atom -> Bool
boolify (INumber int) = int /= 0
boolify (FNumber flt) = flt /= 0
boolify (Symbol str)  = not . null $ str
