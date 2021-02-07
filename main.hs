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

instance Read Exp where
  readsPrec index = (readP_to_S (list <|> atom)) . salt_input
    where openBracket  = char '('
          closeBracket = char ')'
          list         = fmap List $ openBracket >> (manyTill (readS_to_P (reads :: ReadS Exp)) closeBracket) 
          atom         = fmap EAtom $ readS_to_P $ (reads :: ReadS Atom)

instance Read Atom where
  readsPrec index input 
    | null input = []
    | otherwise = [(atom x, intercalate " " xs)]
    where x:xs    = words input
          atom    = fmap (fst . head) $ readP_to_S (int <++ float <++ symbol)
          int     = fmap INumber $ (readS_to_P (reads :: ReadS Int))
          float   = fmap FNumber $ (readS_to_P (reads :: ReadS Float))
          symbol  = fmap Symbol  $ ((readS_to_P (reads :: ReadS String)) <++ (readS_to_P name))

name :: String -> [(String, String)]
name input = [(x, intercalate " " xs)]
  where x:xs = words input
        
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

