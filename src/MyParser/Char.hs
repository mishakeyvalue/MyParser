module MyParser.Char
    (

       anyChar

    ) where

import MyParser (MyParser (..), apply)
import MyParser.Combinators

import Control.Applicative (Alternative (..), Applicative (..))
import Data.Char (digitToInt, isDigit, isLower, isSpace, isUpper)
import Data.List (isPrefixOf)


-- string :: String -> MyParser String
-- string "" = pure ""
-- string str = MyParser f where
--   f inp = foldl g str

anyChar :: MyParser Char
anyChar = satisfy true

space :: MyParser Char
space = satisfy isSpace

spaces :: MyParser ()
spaces = skipMany space

lower :: MyParser Char
lower = satisfy isLower

upper :: MyParser Char
upper = satisfy isUpper

char :: Char -> MyParser Char
char c = satisfy (==c)

digit :: MyParser Int
digit = digitToInt <$> satisfy isDigit

satisfy :: (Char -> Bool) -> MyParser Char
satisfy = satisfy' "Char does not match the predicate"

true :: a -> Bool
true _ = True

satisfy' :: String -> (Char -> Bool) -> MyParser Char
satisfy' err pr = MyParser fun where
  fun "" = Left "Input string is empty"
  fun (c:cs) | pr c = Right (c, cs)
              | otherwise = Left err
