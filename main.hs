import qualified Data.Map as M
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Data.List as L
import Control.Applicative((<|>))
import Data.Monoid

data Env = Env { values :: (M.Map String ExpValue)
               , parent :: (Maybe Env)
               } deriving Show

data ExpValue = EVAtom Atom | EVProcedure Procedure
  deriving (Show)

newtype EvalResult a = EvalResult { getResult :: (a, Env) }
  deriving Show

instance Functor EvalResult where
  fmap f (EvalResult (x, env)) = EvalResult $ (f x, env)

data Exp = EAtom Atom | List [Exp]
  deriving (Eq, Show)

data Atom = INumber Int | FNumber Float | Symbol String
  deriving (Eq, Show)

data Procedure = Procedure { params :: [String]
                           , body :: Env -> EvalResult ExpValue
                           }

instance Show Procedure where
  show _ = "procedure"

p_math op = Procedure ["x", "y"] (\env -> let (EVAtom (INumber x)) = env ! "x"
                                              (EVAtom (INumber y)) = env ! "y"
                                              r                    = (EVAtom (INumber (op x y)))
                                          in EvalResult (r, env)) 

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
        
main = emain global_env

emain :: Env -> IO ()
emain env = do
  line <- getLine
  let (result, new_env) = getResult $ exec env line
  putStrLn $ show result
  emain new_env

exec env input = (eval env) . parse $ input

parse :: String -> Exp
parse = read

eval :: Env -> Exp -> EvalResult ExpValue

eval env (EAtom int@(INumber _))   = EvalResult (EVAtom int, env)
eval env (EAtom float@(FNumber _)) = EvalResult (EVAtom float, env)
eval env (EAtom (Symbol symbol))   = EvalResult (env ! symbol, env)

eval env (List [EAtom (Symbol "if"), test, conseq, alt]) = EvalResult (result, nenv1)
  where (is_test_true, nenv) = (getResult . (fmap boolify) . (eval env)) $ test
        (result, nenv1)      = getResult $ if is_test_true 
                                    then eval nenv conseq
                                    else eval nenv alt

eval env (List [EAtom (Symbol "define"), (EAtom (Symbol symbol)), exp]) = EvalResult (EVAtom $ INumber 1, new_env)
  where (result, nenv) = getResult $ eval env exp
        new_env        = insert nenv symbol result 

eval env (List (proc_name@(EAtom (Symbol _)):proc_args)) = body proc new_env
  where ((EVProcedure proc), nenv) = getResult $ eval env proc_name
        pargs        = zip (params proc) (map (fst . getResult . (eval nenv)) proc_args) -- evaling only with nenv?
        new_env      = Env (M.fromList ((M.toList $ values nenv) ++ pargs)) (parent nenv)

repl :: String -> String
repl = show . parse 

global_env :: Env
global_env = Env (M.fromList $ wrap_ps content) Nothing
  where content = [("+", p_math (+))
                  ,("-", p_math (-))
                  ,("/", p_math (div))
                  ,("*", p_math (*))
                  ]
        wrap_ps = map (\(x, y) -> (x, EVProcedure y))

salt_input :: String -> String
salt_input = concat . map paren
  where paren '(' = "( "
        paren ')' = " )"
        paren c   = [c]

(!) :: Env -> String -> ExpValue
(!) env key = (M.!) (values env) key

insert :: Env -> String -> ExpValue -> Env
insert src key value = Env content (parent src)
  where content = M.insert key value (values src)

boolify :: ExpValue -> Bool
boolify (EVAtom (INumber int)) = int /= 0
boolify (EVAtom (FNumber flt)) = flt /= 0
boolify (EVAtom (Symbol str))  = not . null $ str
