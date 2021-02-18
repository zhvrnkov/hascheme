module Evaler where
import Parser
import qualified Data.Map as M

data Env = Env { values :: (M.Map String ExpValue)
               , parent :: (Maybe Env)
               } deriving Show

data ExpValue = EVAtom Atom | EVProcedure Procedure
  deriving (Show)

newtype EvalResult a = EvalResult { getResult :: (a, Env) }
  deriving Show

instance Functor EvalResult where
  fmap f (EvalResult (x, env)) = EvalResult $ (f x, env)

data Procedure = Procedure { params :: [String]
                           , body :: Env -> EvalResult ExpValue
                           }

instance Show Procedure where
  show _ = "procedure"

eval :: Env -> Exp -> EvalResult ExpValue

eval env (EAtom num@(Number _))  = EvalResult (EVAtom num, env)
eval env (EAtom (Symbol symbol)) = EvalResult (env ! symbol, env)

eval env (EList [EAtom (Symbol "if"), test, conseq, alt]) = EvalResult (result, nenv1)
  where (is_test_true, nenv) = (getResult . (fmap boolify) . (eval env)) $ test
        (result, nenv1)      = getResult $ if is_test_true
                                    then eval nenv conseq
                                    else eval nenv alt

eval env (EList [EAtom (Symbol "define"), (EAtom (Symbol symbol)), exp]) = EvalResult (EVAtom $ Number 1, new_env)
  where (result, nenv) = getResult $ eval env exp
        new_env        = insert nenv symbol result

eval env (EList (proc_name@(EAtom (Symbol _)):proc_args)) = body proc new_env
  where ((EVProcedure proc), nenv) = getResult $ eval env proc_name
        pargs        = zip (params proc) (map (fst . getResult . (eval nenv)) proc_args) -- evaling only with nenv?
        new_env      = Env (M.fromList ((M.toList $ values nenv) ++ pargs)) (parent nenv)

(!) :: Env -> String -> ExpValue
(!) env key = (M.!) (values env) key

insert :: Env -> String -> ExpValue -> Env
insert src key value = Env content (parent src)
  where content = M.insert key value (values src)

boolify :: ExpValue -> Bool
boolify (EVAtom (Number num)) = num /= 0
boolify (EVAtom (Symbol str)) = not . null $ str
