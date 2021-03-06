module Main where
import Parser
import Evaler
import qualified Data.List as L
import qualified Data.Map as M
import Data.Either

p_math op = Procedure ["x", "y"] (\env -> let (EVAtom (Number x)) = env ! "x"
                                              (EVAtom (Number y)) = env ! "y"
                                              r                   = num (op x y)
                                          in EvalResult (r, env))

b_math op = Procedure ["x", "y"] (\env -> let (EVAtom (Number x)) = env ! "x"
                                              (EVAtom (Number y)) = env ! "y"
                                          in EvalResult (if op x y
                                                          then num 1
                                                          else num 0, env))

main = emain global_env

emain :: Env -> IO ()
emain env = do
  line <- getLine
  let (result, new_env) = getResult $ exec env line
  putStrLn $ show result
  emain new_env

exec env input = eval env exp
  where [exp] = (rights $ [parse input])

global_env :: Env
global_env = Env (M.fromList $ wrap_ps content ++ [("foo", EVAtom $ Number 0)]) Nothing
  where content = [("+", p_math (+))
                  ,("-", p_math (-))
                  ,("/", p_math (div))
                  ,("*", p_math (*))
                  ,("<", b_math (<))
                  ,(">", b_math (>))
                  ,("=", b_math (==))
                  ,(">=", b_math (>=))
                  ,("<=", b_math (<=))
                  ]
        wrap_ps = map (\(x, y) -> (x, EVProcedure y))

num :: Int -> ExpValue
num = EVAtom . Number
